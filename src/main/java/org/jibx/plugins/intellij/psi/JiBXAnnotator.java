package org.jibx.plugins.intellij.psi;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.psi.*;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PropertyUtil;
import com.intellij.psi.xml.XmlAttribute;
import com.intellij.psi.xml.XmlTag;
import static java.lang.String.format;

public class JiBXAnnotator implements Annotator {
    private final Module module;
    private static final Logger logger = Logger.getInstance(JiBXAnnotator.class.getName());

    public JiBXAnnotator(Module module) {
        this.module = module;
        logger.warn(format("Activating JiBX annotations for module %s", module.getName()));
    }

    public void annotate(PsiElement psiElement, AnnotationHolder holder) {
        if (psiElement instanceof XmlAttribute) {
            XmlAttribute attribute = (XmlAttribute) psiElement;
            XmlTag tag = attribute.getParent();
            XmlTag parent = tag.getParentTag();
            if ("field".equals(attribute.getName()) && "value".equals(tag.getName())
                    && parent != null && "mapping".equals(parent.getName())) {
                String clazz = parent.getAttributeValue("class");
                XmlAttribute clazzAttribute = parent.getAttribute("class", "");
                assert clazzAttribute != null;
                if (clazz == null) {
                    holder.createErrorAnnotation(clazzAttribute, "The class should not be empty!");
                    return;
                }
                Project project = module.getProject();
                GlobalSearchScope scope = module.getModuleWithDependenciesAndLibrariesScope(true);
                PsiClass psiClass = JavaPsiFacade.getInstance(project).findClass(clazz, scope);
                if (psiClass == null) {
                    logger.warn(format("Could not find class %s in module %s", clazz, module.getName()));
                    holder.createErrorAnnotation(clazzAttribute, "The class does not exist!");
                    return;
                }
                String property = attribute.getValue();
                if (property == null || "".equals(property.trim())) {
                    holder.createErrorAnnotation(attribute, "Property name can't be an empty string");
                    return;
                }
                PsiMethod getter = PropertyUtil.findPropertyGetter(psiClass, property, false, true);
                if (getter == null) {
                    holder.createErrorAnnotation(attribute,
                            format("Property '%s' can't be found on class '%s'", property, clazz));
                }
            }
        }
    }
}
