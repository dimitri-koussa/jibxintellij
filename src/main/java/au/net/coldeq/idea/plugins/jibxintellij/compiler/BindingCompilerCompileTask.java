package au.net.coldeq.idea.plugins.jibxintellij.compiler;

import static com.intellij.openapi.compiler.CompilerMessageCategory.ERROR;
import static com.intellij.openapi.compiler.CompilerMessageCategory.INFORMATION;
import static java.util.Arrays.asList;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import org.jibx.binding.Compile;
import org.jibx.binding.classes.ClassCache;
import org.jibx.binding.classes.ClassFile;
import org.jibx.binding.model.BindingElement;
import org.jibx.binding.model.ValidationContext;
import org.jibx.binding.model.ValidationProblem;
import org.jibx.runtime.BindingDirectory;
import org.jibx.runtime.JiBXException;

import com.intellij.codeInsight.dataflow.SetUtil;
import com.intellij.ide.highlighter.JavaFileType;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.compiler.CompileContext;
import com.intellij.openapi.compiler.CompileTask;
import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.roots.CompilerModuleExtension;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.libraries.LibraryUtil;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.PathUtil;

public class BindingCompilerCompileTask implements CompileTask {

    // Buffer size for copying stream input
    private static final int COPY_BUFFER_SIZE = 1024;

    private Module module;
    private Logger logger = Logger.getInstance(getClass());

    public BindingCompilerCompileTask(Module module) {
        this.module = module;
    }

    @Override
    public boolean execute(final CompileContext compileContext) {
        if (compileContext.getMessageCount(ERROR) > 0) {
            return true;
        }
        String[] projectPaths = ApplicationManager.getApplication().runReadAction(new ProjectPathsFinder());
        String output = ApplicationManager.getApplication().runReadAction(new OutputPathsFinder(compileContext));
        String[] libraries = ApplicationManager.getApplication().runReadAction(new LibraryPathsFinder());
        String[] modules = ApplicationManager.getApplication().runReadAction(new ModulePathsFinder());
        String testOutput = ApplicationManager.getApplication().runReadAction(new TargetPathFinder(compileContext));
        String result = ApplicationManager.getApplication().runReadAction(new BindingCompilerComputation(compileContext, projectPaths, output, testOutput, libraries, modules));

        // Abort the compilation process if there is an error compiling the binding
        return result.equals(Boolean.TRUE.toString());
    }

    private class BindingCompilerComputation implements Computable<String> {
        private final CompileContext compileContext;
        private final String[] projectPaths;
        private final String output;
        private final String testOutput;
        private final String[] libraries;
        private final String[] modules;

        private BindingCompilerComputation(CompileContext compileContext, String[] projectPaths, String output, String testOutput, String[] libraries, String[] modules) {
            this.compileContext = compileContext;
            this.projectPaths = projectPaths;
            this.output = output;
            this.testOutput = testOutput;
            this.libraries = libraries;
            this.modules = modules;
        }

        @Override
        public String compute() {
            ValidationContext vctx = null;
            Set<VirtualFile> bindings = module.getComponent(au.net.coldeq.idea.plugins.jibxintellij.compiler.BindingCompilerModuleComponent.class).getBindings();

            try {
                compileContext.addMessage(INFORMATION, "Compiling JiBX bindings...", null, 0, 0);

                if (bindings.size() == 0) {
                    compileContext.addMessage(INFORMATION, "No JiBX bindings found, nothing to do.", null, 0, 0);
                    return Boolean.TRUE.toString();
                }
                compileContext.addMessage(INFORMATION, "Number of JiBX bindings found: " + bindings.size(), null, 0, 0);

                Set<String> namesOfClassesFilesThatAreGoingToBeCompiled = convertFilesToClassNames(this.compileContext.getCompileScope().getFiles(JavaFileType.INSTANCE, true));
                Set<String> namesOfClassesThatAreJibxBound;
                try {
                    namesOfClassesThatAreJibxBound = figureOutWhichClassesAreJibxBound(bindings);
                } catch (IOException e) {
                    compileContext.addMessage(ERROR, String.format("Unable to read file for jibx post-compile tasks: %s", e.getMessage()), bindings.toString(), 0, 0);
                    return Boolean.FALSE.toString();
                } catch (XMLStreamException e) {
                    compileContext.addMessage(ERROR, String.format("Unable to parse xml for jibx post-compile tasks: %s", e.getMessage()), bindings.toString(), 0, 0);
                    return Boolean.FALSE.toString();
                }

                Set<String> jibxBoundClassesThatHaveBeenCompiled = SetUtil.intersect(namesOfClassesFilesThatAreGoingToBeCompiled, namesOfClassesThatAreJibxBound);
                if (jibxBoundClassesThatHaveBeenCompiled.isEmpty()) {
                    compileContext.addMessage(INFORMATION, "No need to re-run any jibx bindings", bindings.toString(), 0, 0);
                    return Boolean.TRUE.toString();
                } else {
                    compileContext.addMessage(INFORMATION, "jibx bound classes have been compiled: " + Arrays.toString(jibxBoundClassesThatHaveBeenCompiled.toArray()), null, 0, 0);
                }

                List<String> paths = new ArrayList<>();
                paths.addAll(asList(
                        output,
                        PathUtil.getJarPathForClass(Compile.class),
                        PathUtil.getJarPathForClass(BindingDirectory.class)
                ));
                paths.addAll(asList(this.modules));
                paths.addAll(asList(this.libraries));
                if (testOutput != null) {
                    paths.add(testOutput);
                }
                paths.addAll(asList(projectPaths));

                String[] bindingsPaths = findBindings(bindings);
                vctx = validateBindings(bindings, paths);

                runJiBXCompiler(paths, bindingsPaths);
                compileContext.addMessage(INFORMATION, "JiBX binding compilation successful", null, 0, 0);
                return Boolean.TRUE.toString();
            } catch (JiBXException e) {
                logJiBXExceptionToIdeaErrorConsole(vctx, e, compileContext);
                return Boolean.FALSE.toString();
            } catch (Throwable t) {
                logger.error("Unexpected global exception", t);
                compileContext.addMessage(ERROR, String.format("Unexpected global exception: %s", t), null, 0, 0);
                return Boolean.FALSE.toString();
            }
        }

        private Set<String> figureOutWhichClassesAreJibxBound(Set<VirtualFile> bindings) throws IOException, XMLStreamException {
            Set<String> results = new HashSet<>();
            for (VirtualFile binding : bindings) {
                String classFoundInBindingXml = figureOutWhichClassIsJibxBound(binding);
                if (classFoundInBindingXml != null) {
                    results.add(classFoundInBindingXml);
                }
            }
            return results;
        }

        private String figureOutWhichClassIsJibxBound(VirtualFile binding) throws IOException, XMLStreamException {
            XMLInputFactory xmlInFact = XMLInputFactory.newInstance();
            XMLStreamReader reader = xmlInFact.createXMLStreamReader(binding.getInputStream());
            while (reader.hasNext()) {
                reader.next();
                if (reader.isStartElement() && reader.hasName() && "mapping".equals(reader.getName().getLocalPart())) {
                    return reader.getAttributeValue(null, "class");
                }
            }
            return null;
        }

        private Set<String> convertFilesToClassNames(VirtualFile[] files) {
            Set<String> results = new HashSet<>();
            for (VirtualFile file : files) {
                String canonicalPath = file.getCanonicalPath();
                if (canonicalPath.matches(".*src/[^/]*/java/.*")) {
                    String className = canonicalPath.split("src/[^/]*/java/")[1].replaceAll("/", ".").replaceAll("\\.java", "");
                    results.add(className);
                }
            }
            return results;
        }

        private ValidationContext validateBindings(Set<VirtualFile> bindings, List<String> paths) throws IOException {
            ClassCache.setPaths(paths.toArray(new String[paths.size()]));
            ClassFile.setPaths(paths.toArray(new String[paths.size()]));
            ValidationContext vctx = BindingElement.newValidationContext();

            Iterator vfIterator = bindings.iterator();
            while (vfIterator.hasNext()) {
                VirtualFile binding = (VirtualFile) vfIterator.next();
                String bindingPath = binding.getPath();
                try {
                    BindingElement.validateBinding(binding.getPath(), new URL(binding.getUrl()), new ByteArrayInputStream(getStreamData(new FileInputStream(bindingPath))), vctx);
                } catch (JiBXException e) {
                    compileContext.addMessage(ERROR, String.format("Unexpected exception while validating binding %s: %s", bindingPath, e.getMessage()), null, 0, 0);
                }
            }
            return vctx;
        }
    }

    private String[] findBindings(Set<VirtualFile> bindings) {
        String[] bindingsPaths = new String[bindings.size()];
        int i = 0;
        for (Iterator vfIterator = bindings.iterator(); vfIterator.hasNext(); i++) {
            VirtualFile binding = (VirtualFile) vfIterator.next();
            bindingsPaths[i] = binding.getPath();
        }
        return bindingsPaths;
    }

    private void runJiBXCompiler(List<String> paths, String[] bindingsPaths) throws JiBXException {
        Compile compiler = new Compile();
        compiler.setLoad(false);
        compiler.setVerbose(false);
        compiler.setVerify(false);
        compiler.compile(paths.toArray(new String[paths.size()]), bindingsPaths);
    }

    private void logJiBXExceptionToIdeaErrorConsole(ValidationContext vctx, JiBXException e, CompileContext compileContext) {
        if (vctx == null) {
            compileContext.addMessage(ERROR, e.getMessage(), null, 0, 0);
        } else {
            List<ValidationProblem> problems = vctx.getProblems();
            for (ValidationProblem problem : problems) {
                CompilerMessageCategory severity = ERROR;
                switch (problem.getSeverity()) {
                    case ValidationProblem.WARNING_LEVEL:
                        severity = CompilerMessageCategory.WARNING;
                        break;
                    case ValidationProblem.ERROR_LEVEL:
                        severity = ERROR;
                        break;
                    case ValidationProblem.FATAL_LEVEL:
                        severity = ERROR;
                        break;
                }
                String message = problem.getDescription();
                int beforeLine = message.indexOf("line ") + 5;
                int afterLine = message.indexOf(", ", beforeLine);
                int line = Integer.parseInt(message.substring(beforeLine, afterLine));
                int beforeCol = message.indexOf(", col ", afterLine) + 6;
                int afterCol = message.indexOf(", in", beforeCol);
                int col = Integer.parseInt(message.substring(beforeCol, afterCol));
                int beforeFile = message.indexOf(" in ", afterCol) + 4;
                int afterFile = message.indexOf(")", beforeFile);
                String file = "file://" + message.substring(beforeFile, afterFile);
                compileContext.addMessage(severity, "JiBX: " + message, file, line, col);
            }
        }
    }

    private class TargetPathFinder implements Computable<String> {

        private CompileContext compileContext;

        private TargetPathFinder(CompileContext compileContext) {
            this.compileContext = compileContext;
        }

        @Override
        public String compute() {
            try {
                return new URL(CompilerModuleExtension.getInstance(module).getCompilerOutputUrl()).getFile();
            } catch (MalformedURLException e) {
                compileContext.addMessage(ERROR, "Can't find test output directory", null, 0, 0);
                logger.error("Can't find test output directory", e);
            }
            return null;
        }
    }

    private class OutputPathsFinder implements Computable<String> {

        private final CompileContext compileContext;

        public OutputPathsFinder(CompileContext compileContext) {
            this.compileContext = compileContext;
        }

        @Override
        public String compute() {
            try {
                return new URL(CompilerModuleExtension.getInstance(module).getCompilerOutputUrl()).getFile();
            } catch (MalformedURLException e) {
                compileContext.addMessage(ERROR, "Can't find output directory", null, 0, 0);
                logger.error("Can't find output directory", e);
            }
            return null;
        }
    }

    private class LibraryPathsFinder implements Computable<String[]> {

        @Override
        public String[] compute() {
            VirtualFile[] libraryRoots = LibraryUtil.getLibraryRoots(new Module[]{module}, false, false);
            String[] paths = new String[libraryRoots.length];
            for (int i = 0; i < libraryRoots.length; i++) {
                VirtualFile virtualFile = libraryRoots[i];
                String path = virtualFile.getPath();
                if (path.endsWith("!/")) {
                    path = path.substring(0, virtualFile.getPath().length() - 2);
                }
                paths[i] = path;
            }
            return paths;
        }
    }

    private class ModulePathsFinder implements Computable<String[]> {

        @Override
        public String[] compute() {
            try {
                Set<Module> moduleDependencies = new HashSet<>();
                ModuleUtilCore.getDependencies(module, moduleDependencies);
                moduleDependencies.remove(module);

                Set<String> paths = new HashSet<>();
                for (Module dependentModule : moduleDependencies) {
                    paths.add(new URL(CompilerModuleExtension.getInstance(dependentModule).getCompilerOutputUrl()).getFile());
                }
                return paths.toArray(new String[paths.size()]);
            } catch (MalformedURLException e) {
                throw new RuntimeException(e);
            }
        }
    }

    private class ProjectPathsFinder implements Computable<String[]> {
        @Override
        public String[] compute() {
            ModuleRootManager rootManager = ModuleRootManager.getInstance(module);
            VirtualFile[] projectClasspath = rootManager.getSourceRoots();
            String[] paths = new String[projectClasspath.length];
            for (int i = 0; i < projectClasspath.length; i++) {
                VirtualFile virtualFile = projectClasspath[i];
                String path = virtualFile.getPath();
                if (path.endsWith("!/")) {
                    path = path.substring(0, virtualFile.getPath().length() - 2);
                }
                paths[i] = path;
            }
            return paths;
        }
    }

    private static byte[] getStreamData(InputStream is) throws IOException {
        byte[] buff = new byte[COPY_BUFFER_SIZE];
        try (ByteArrayOutputStream os = new ByteArrayOutputStream()) {
            int count;
            while ((count = is.read(buff)) >= 0) {
                os.write(buff, 0, count);
            }
            return os.toByteArray();
        }
    }
}
