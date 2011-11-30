package org.jibx.plugins.intellij.psi;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleComponent;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.resolve.reference.ReferenceProvidersRegistry;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.xml.XmlAttribute;
import com.intellij.psi.xml.XmlAttributeValue;
import com.intellij.psi.xml.XmlTag;
import com.intellij.util.ProcessingContext;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class JiBXReferencesToJavaCode implements ModuleComponent {
	private final JavaPsiFacade javaPsiFacade;
	private final GlobalSearchScope scope;
	private final ReferenceProvidersRegistry registry;

	public JiBXReferencesToJavaCode(final Module module, final JavaPsiFacade javaPsiFacade, ReferenceProvidersRegistry registry) {
		this.javaPsiFacade = javaPsiFacade;
		this.registry = registry;
		this.scope = module.getModuleWithDependenciesAndLibrariesScope(true);
	}

	public void projectOpened() { }

	public void projectClosed() { }

	public void moduleAdded() { }

	@NotNull
	public String getComponentName() {
		return "JiBXReferencesToJavaCode";
	}

	public void initComponent() {
		registry.registerReferenceProvider(XmlAttributeValue.class, new PsiReferenceProvider() {
			@NotNull
			public PsiReference[] getReferencesByElement(@NotNull PsiElement psiElement, @NotNull ProcessingContext ctx) {
				XmlAttributeValue attributeValue = (XmlAttributeValue) psiElement;
				if (!(attributeValue.getParent() instanceof XmlAttribute)) {
					return PsiReference.EMPTY_ARRAY;
				}
				XmlAttribute attribute = (XmlAttribute) attributeValue.getParent();
				// build the references list
				List<PsiReference> references = new ArrayList<PsiReference>();
				if ("field".equals(attribute.getName())) {
					references.addAll(getReferencesForField(attribute));
				} else if ("test-method".equals(attribute.getName())) {
					references.addAll(getReferencesForTestMethod(attribute));
				} else if ("get-method".equals(attribute.getName())) {
					references.addAll(getReferencesForGetMethod(attribute));
				} else if ("set-method".equals(attribute.getName())) {
					references.addAll(getReferencesForSetMethod(attribute));
				} else if ("factory".equals(attribute.getName())) {
					references.addAll(getReferencesForFactoryMethod(attribute));
				}
				return references.toArray(new PsiReference[references.size()]);
			}

			private List<? extends PsiReference> getReferencesForField(XmlAttribute attribute) {
				final PsiClass psiClass = findClassForFieldOrMethod(attribute);
				if (psiClass == null) {
					return Collections.emptyList();
				}
				return Collections.singletonList(new FieldReference(attribute, psiClass));
			}

			private List<? extends PsiReference> getReferencesForTestMethod(XmlAttribute attribute) {
				final PsiClass psiClass = findClassForFieldOrMethod(attribute);
				if (psiClass == null) {
					return Collections.emptyList();
				}
				return Collections.singletonList(new TestMethodReference(attribute, psiClass));
			}

			private List<? extends PsiReference> getReferencesForGetMethod(XmlAttribute attribute) {
				final PsiClass psiClass = findClassForFieldOrMethod(attribute);
				if (psiClass == null) {
					return Collections.emptyList();
				}
				return Collections.singletonList(new GetMethodReference(attribute, psiClass));
			}

			private List<? extends PsiReference> getReferencesForSetMethod(XmlAttribute attribute) {
				final PsiClass psiClass = findClassForFieldOrMethod(attribute);
				if (psiClass == null) {
					return Collections.emptyList();
				}
				return Collections.singletonList(new SetMethodReference(attribute, psiClass));
			}

			private List<? extends PsiReference> getReferencesForFactoryMethod(XmlAttribute attribute) {
				String clazz = attribute.getValue().substring(0, attribute.getValue().lastIndexOf('.'));
				final PsiClass psiClass = javaPsiFacade.findClass(clazz, scope);
				if (psiClass == null) {
					return Collections.emptyList();
				}
				return Collections.singletonList(new FactoryMethodReference(attribute, psiClass));
			}

			private PsiClass findClassForFieldOrMethod(XmlAttribute attribute) {
				if (attribute == null) {
					return null;
				}
				XmlTag tag = attribute.getParent();
				if (tag == null) {
					return null;
				}
				String clazz;
				do {
					clazz = tag.getAttributeValue("class");
					tag = tag.getParentTag();
				} while (clazz == null && tag != null);
				if (clazz != null) {
					return javaPsiFacade.findClass(clazz, scope);
				} else {
					return null;
				}
			}
		});
	}

	public void disposeComponent() {
	}
}
