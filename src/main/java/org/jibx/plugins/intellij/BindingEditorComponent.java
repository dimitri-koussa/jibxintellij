/*
 * Copyright (c) 2005-2007, Kalixia, SARL. All Rights Reserved.
 */
package org.jibx.plugins.intellij;

import com.intellij.lang.StdLanguages;
import com.intellij.lang.LanguageAnnotators;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleComponent;
import org.jetbrains.annotations.NotNull;
import org.jibx.plugins.intellij.psi.JiBXAnnotator;

/**
 * @author Jerome Bernard (jerome.bernard@kalixia.com)
 */
public class BindingEditorComponent implements ModuleComponent {
    private Module module;
    private JiBXAnnotator annotator;
    private boolean shouldActivate = false;

    public BindingEditorComponent(Module module, BindingCompilerModuleComponent binder) {
        this.module = module;
        if (binder.getBindings().size() > 0)
            shouldActivate = true;
    }

    public void projectOpened() {
    }

    public void projectClosed() {
    }

    public void moduleAdded() {
    }

    @NotNull
    public String getComponentName() {
        return "JiBX binding editor";
    }

    public void initComponent() {
        if (shouldActivate) {
            annotator = new JiBXAnnotator(module);
            LanguageAnnotators.INSTANCE.addExpicitExtension(StdLanguages.XML, annotator);
        }
    }

    public void disposeComponent() {
        if (shouldActivate)
            LanguageAnnotators.INSTANCE.removeExpicitExtension(StdLanguages.XML, annotator);
    }

}