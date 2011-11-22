/*
 * Copyright (c) 2005-2007, Kalixia, SARL. All Rights Reserved.
 */
package org.jibx.plugins.intellij.actions;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.DataConstants;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import org.apache.log4j.Logger;
import org.jibx.plugins.intellij.BindingCompilerModuleComponent;

/**
 * Add a JiBX mapping to the list of mappings.
 * @author Jerome Bernard (jerome.bernard@kalixia.com)
 */
public class AddMappingAction extends AnAction {
    private Logger logger = Logger.getLogger(getClass());

    public void actionPerformed(AnActionEvent event) {
        PsiFile psiBinding = (PsiFile) event.getDataContext().getData(DataConstants.PSI_FILE);
        Module module = (Module) event.getDataContext().getData(DataConstants.MODULE);
        if (psiBinding == null)
            return;
        VirtualFile binding = psiBinding.getVirtualFile();
        if (binding == null)
            return;
        assert module != null;
        BindingCompilerModuleComponent compiler = module.getComponent(BindingCompilerModuleComponent.class);
        compiler.addBinding(binding);

        if (logger.isInfoEnabled())
            logger.info(String.format("Added JiBX binding: %s", binding.getPath()));
    }
}
