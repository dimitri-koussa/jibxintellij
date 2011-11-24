/******************************************************************************
 * Copyright (c) 2005-2005                                                    *
 *     Kalixia, SARL.  All rights reserved.                                   *
 *                                                                            *
 * See the file LICENSE for redistribution information.                       *
 ******************************************************************************/
package org.jibx.plugins.intellij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.DataConstants;
import com.intellij.openapi.module.Module;
import com.intellij.psi.PsiFile;
import org.apache.log4j.Logger;
import org.jibx.plugins.intellij.BindingCompilerModuleComponent;

/**
 * Delete a JiBX mapping to the list of mappings.
 * @author Jerome Bernard (jerome.bernard@kalixia.com)
 */
public class DeleteMappingAction extends AnAction {
    private Logger logger = Logger.getLogger(getClass());

    public void actionPerformed(AnActionEvent event) {
        PsiFile psiBinding = (PsiFile) event.getDataContext().getData(DataConstants.PSI_FILE);
        Module module = (Module) event.getDataContext().getData(DataConstants.MODULE);
        if (psiBinding == null || psiBinding.getVirtualFile() == null)
            return;
        module.getComponent(BindingCompilerModuleComponent.class).removeBinding(psiBinding.getVirtualFile());
        logger.info("Deleted JiBX binding: " + psiBinding.getVirtualFile().getPath());
    }
}
