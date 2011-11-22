package org.jibx.plugins.intellij.psi;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PropertyUtil;
import com.intellij.psi.xml.XmlAttribute;
import com.intellij.psi.xml.XmlAttributeValue;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

class GetMethodReference implements PsiReference {
    private final PsiClass psiClass;
    private final XmlAttribute fieldAttribute;
    private final XmlAttributeValue fieldAttributeValue;
    private final PsiMethod[] methods;

    public GetMethodReference(final XmlAttribute fieldAttribute, final PsiClass psiClass) {
        this.psiClass = psiClass;
        this.fieldAttribute = fieldAttribute;
        this.fieldAttributeValue = fieldAttribute.getValueElement();
        this.methods = findGetters(psiClass.getAllMethods());
    }

    public PsiElement getElement() {
        return fieldAttributeValue;
    }

    public TextRange getRangeInElement() {
        return new TextRange(1, getElement().getTextRange().getLength() - 1);
    }

    @Nullable
    public PsiElement resolve() {
        String value = fieldAttributeValue.getValue();
        PsiMethod[] completions = findGetters(psiClass.findMethodsByName(value, true));
        PsiMethod method = null;
        // if we didn't find the field, search for one whose name starts with the value typed
        switch (completions.length) {
            case 0:
                method = null;
                break;
            case 1:
                method = completions[0];
                break;
            default:
                for (PsiMethod test : methods) {
                    if (test.getName().startsWith(value))
                        method = psiClass.findMethodsByName(value, true)[0];
                }
        }
        return method;
    }

    public String getCanonicalText() {
        return fieldAttributeValue.getValue();
    }

    public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
        fieldAttribute.setValue(newElementName);
        return fieldAttributeValue;
    }

    public PsiElement bindToElement(@NotNull PsiElement element) throws IncorrectOperationException {
        throw new IncorrectOperationException();
    }

    public boolean isReferenceTo(PsiElement element) {
        return resolve() == element;
    }

    public Object[] getVariants() {
        return methods;
    }

    public boolean isSoft() {
        return false;
    }

    private PsiMethod[] findGetters(PsiMethod[] methods) {
        List<PsiMethod> filtered = new ArrayList<PsiMethod>(methods.length);
        for (PsiMethod allMethod : methods)
            if (PropertyUtil.isSimplePropertyGetter(allMethod))
                filtered.add(allMethod);
        return filtered.toArray(new PsiMethod[filtered.size()]);
    }
}