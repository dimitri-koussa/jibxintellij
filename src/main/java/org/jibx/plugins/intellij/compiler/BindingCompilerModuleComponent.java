/*
 * Copyright (c) 2005-2007, Kalixia, SARL. All Rights Reserved.
 */
package org.jibx.plugins.intellij.compiler;

import com.intellij.openapi.compiler.CompilerManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleComponent;
import com.intellij.openapi.util.InvalidDataException;
import com.intellij.openapi.util.JDOMExternalizable;
import com.intellij.openapi.util.WriteExternalException;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import org.apache.log4j.Logger;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;

import java.util.HashSet;
import java.util.Set;

/**
 * @author Jerome Bernard (jerome.bernard@kalixia.com)
 */
public class BindingCompilerModuleComponent implements ModuleComponent, JDOMExternalizable {
	private Module module;
	private Set<VirtualFile> bindings = new HashSet<VirtualFile>();
	private Logger logger = Logger.getLogger(getClass());

	public void addBinding(VirtualFile binding) {
		bindings.add(binding);
	}

	public void removeBinding(VirtualFile binding) {
		bindings.remove(binding);
	}

	public Set<VirtualFile> getBindings() {
		return bindings;
	}

	public BindingCompilerModuleComponent(Module module) {
		this.module = module;
	}

	public void projectOpened() { }

	public void projectClosed() { }

	public void moduleAdded() {
		CompilerManager.getInstance(module.getProject()).addAfterTask(new BindingCompilerCompileTask(module));
	}

	@NotNull
	public String getComponentName() {
		return "JiBX binding compiler";
	}

	public void initComponent() { }

	public void disposeComponent() { }

	public void readExternal(Element element) throws InvalidDataException {
		logger.info("Reading settings...");
		for (Object o : element.getChildren("mapping")) {
			Element mapping = (Element) o;
			bindings.add(LocalFileSystem.getInstance().findFileByPath(mapping.getValue()));
			if (logger.isInfoEnabled()) {
				logger.info(String.format("Added binding: %s", mapping.getValue()));
			}
		}
	}

	public void writeExternal(Element element) throws WriteExternalException {
		for (VirtualFile binding : bindings) {
			element.addContent(new Element("mapping").addContent(binding.getPath()));
		}
	}
}
