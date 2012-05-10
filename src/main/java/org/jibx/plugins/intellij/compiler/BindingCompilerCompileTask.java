package org.jibx.plugins.intellij.compiler;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.compiler.CompileContext;
import com.intellij.openapi.compiler.CompileTask;
import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.roots.CompilerModuleExtension;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.PathUtil;
import org.jibx.binding.Compile;
import org.jibx.binding.classes.ClassCache;
import org.jibx.binding.classes.ClassFile;
import org.jibx.binding.model.BindingElement;
import org.jibx.binding.model.ValidationContext;
import org.jibx.binding.model.ValidationProblem;
import org.jibx.runtime.BindingDirectory;
import org.jibx.runtime.JiBXException;

import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;

public class BindingCompilerCompileTask implements CompileTask {

    // buffer size for copying stream input
    private static final int COPY_BUFFER_SIZE = 1024;

    private Module module;
    private Logger logger = Logger.getInstance(getClass());

    public BindingCompilerCompileTask(Module module) {
        this.module = module;
    }

    @Override
    public boolean execute(final CompileContext compileContext) {
        logger.info("Executing BindingCompilerCompileTask...");
        String[] projectPaths = ApplicationManager.getApplication().runReadAction(new ProjectPathsFinder());
        String output = ApplicationManager.getApplication().runReadAction(new OutputPathsFinder(compileContext));
        String testOutput = ApplicationManager.getApplication().runReadAction(new TargetPathFinder(compileContext));
        String result = ApplicationManager.getApplication().runReadAction(new BindingCompilerComputation(compileContext, projectPaths, output, testOutput));

        // abort the compilation process if there is an error compiling the binding
        return result.equals(Boolean.TRUE.toString());
    }

    private class BindingCompilerComputation implements Computable<String> {
        private final CompileContext compileContext;
        private final String[] projectPaths;
        private final String output;
        private final String testOutput;

        private BindingCompilerComputation(CompileContext compileContext, String[] projectPaths, String output, String testOutput) {
            this.compileContext = compileContext;
            this.projectPaths = projectPaths;
            this.output = output;
            this.testOutput = testOutput;
        }

        // return null if there is a problem compiling the bindings
        @Override
        public String compute() {
            ValidationContext vctx = null;
            // retrieve the bindings
            Set<VirtualFile> bindings = module.getComponent(BindingCompilerModuleComponent.class).getBindings();

            try {
                compileContext.addMessage(CompilerMessageCategory.INFORMATION, "Compiling JiBX binding...", null, 0, 0);

                // check if at least one binding can be found
                if (bindings.size() == 0) {
                    return Boolean.TRUE.toString();
                }

                List<String> paths = new ArrayList<String>();
                paths.addAll(Arrays.asList(output, PathUtil.getJarPathForClass(Compile.class), PathUtil.getJarPathForClass(BindingDirectory.class)));
                if (testOutput != null) {
                    paths.add(testOutput);
                }
                paths.addAll(Arrays.asList(projectPaths));

                String[] bindingsPaths = findBindings(bindings);
                vctx = validateBindings(vctx, bindings, paths);

                runJiBXCompiler(paths, bindingsPaths);
                compileContext.addMessage(CompilerMessageCategory.INFORMATION, "JiBX binding compilation successful", null, 0, 0);
                logger.info("Finished JiBX bind...");
                return Boolean.TRUE.toString();
            } catch (JiBXException e) {
                logJiBXExceptionToIdeaErrorConsole(vctx, e, compileContext);
                return Boolean.FALSE.toString();
            } catch (Throwable t) {
                logger.error("Unexpected global exception", t);
                compileContext.addMessage(CompilerMessageCategory.ERROR, String.format("Unexpected global exception: %s", t), null, 0, 0);
                return Boolean.FALSE.toString();
            }
        }

        private ValidationContext validateBindings(ValidationContext vctx, Set<VirtualFile> bindings, List<String> paths) throws IOException {
            ClassCache.setPaths(paths.toArray(new String[paths.size()]));
            ClassFile.setPaths(paths.toArray(new String[paths.size()]));
            vctx = BindingElement.newValidationContext();
            int i = 0;
            for (Iterator vfIterator = bindings.iterator(); vfIterator.hasNext(); i++) {
                VirtualFile binding = (VirtualFile) vfIterator.next();
                String bindingPath = binding.getPath();
                logger.info(String.format("Using binding: %s", bindingPath));
                // validate the binding definition
                try {
                    BindingElement.validateBinding(binding.getPath(), new URL(binding.getUrl()), new ByteArrayInputStream(getStreamData(new FileInputStream(bindingPath))), vctx);
                } catch (JiBXException e) {
                    compileContext.addMessage(CompilerMessageCategory.ERROR, String.format("Unexpected exception while validating binding %s: %s", bindingPath, e.getMessage()), null, 0, 0);
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
            logger.info(String.format("Using binding: %s", bindingsPaths[i]));
        }
        return bindingsPaths;
    }

    private void runJiBXCompiler(List<String> paths, String[] bindingsPaths) throws JiBXException {// run the binding compiler
        Compile compiler = new Compile();
        compiler.setLoad(false);
        compiler.setVerbose(true);
        compiler.setVerify(true);
        compiler.compile(paths.toArray(new String[paths.size()]), bindingsPaths);
    }

    private void logJiBXExceptionToIdeaErrorConsole(ValidationContext vctx, JiBXException e, CompileContext compileContext) {
        if (vctx == null) {
            compileContext.addMessage(CompilerMessageCategory.ERROR, e.getMessage(), null, 0, 0);
        } else {
            List<ValidationProblem> problems = vctx.getProblems();
            for (ValidationProblem problem : problems) {
                CompilerMessageCategory severity = CompilerMessageCategory.ERROR;
                switch (problem.getSeverity()) {
                    case ValidationProblem.WARNING_LEVEL:
                        severity = CompilerMessageCategory.WARNING;
                        break;
                    case ValidationProblem.ERROR_LEVEL:
                        severity = CompilerMessageCategory.ERROR;
                        break;
                    case ValidationProblem.FATAL_LEVEL:
                        severity = CompilerMessageCategory.ERROR;
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
                compileContext.addMessage(CompilerMessageCategory.ERROR, "Can't find test output directory", null, 0, 0);
                logger.error("Cant' find test output directory", e);
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
                compileContext.addMessage(CompilerMessageCategory.ERROR, "Can't find output directory", null, 0, 0);
                logger.error("Cant' find output directory", e);
            }
            return null;
        }
    }

    private class ProjectPathsFinder implements Computable<String[]> {
        @Override
        public String[] compute() {
            String[] paths;
            ModuleRootManager rootManager = ModuleRootManager.getInstance(module);
            VirtualFile[] projectClasspath = rootManager.getFiles(OrderRootType.COMPILATION_CLASSES);
            paths = new String[projectClasspath.length];
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
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        int count;
        while ((count = is.read(buff)) >= 0) {
            os.write(buff, 0, count);
        }
        return os.toByteArray();
    }
}
