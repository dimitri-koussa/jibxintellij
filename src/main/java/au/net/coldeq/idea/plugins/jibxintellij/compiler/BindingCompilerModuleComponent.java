package au.net.coldeq.idea.plugins.jibxintellij.compiler;

import com.intellij.openapi.compiler.CompilerManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleComponent;
import com.intellij.openapi.roots.ContentIterator;
import com.intellij.openapi.roots.ModuleFileIndex;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.startup.StartupManager;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;

import java.util.HashSet;
import java.util.Set;

public class BindingCompilerModuleComponent implements ModuleComponent {
    private Module module;
    private Set<VirtualFile> bindings = new HashSet<VirtualFile>();
    private Logger logger = Logger.getInstance(getClass());

    public BindingCompilerModuleComponent(Module module) {
        this.module = module;
    }

    @Override
    @NotNull
    public String getComponentName() {
        return "JiBX binding compiler";
    }

    public Set<VirtualFile> getBindings() {
        return bindings;
    }

    @Override
    public void initComponent() {
        StartupManager.getInstance(module.getProject()).runWhenProjectIsInitialized(new Runnable() {
            @Override
            public void run() {
                ModuleFileIndex fileIndex = ModuleRootManager.getInstance(module).getFileIndex();
                fileIndex.iterateContent(new ContentIterator() {
                    @Override
                    public boolean processFile(VirtualFile fileOrDir) {
                        if (fileOrDir.getPath().matches("^.*/src/main/.*jibx.*/.*\\.xml$")) {
                            bindings.add(fileOrDir);
                        }
                        return true;
                    }
                });
            }
        });
    }

    @Override
    public void projectOpened() {}

    @Override
    public void projectClosed() {}

    @Override
    public void moduleAdded() {
        CompilerManager.getInstance(module.getProject()).addAfterTask(new BindingCompilerCompileTask(module));
    }

    @Override
    public void disposeComponent() {}
}
