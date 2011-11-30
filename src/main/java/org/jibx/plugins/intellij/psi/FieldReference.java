package org.jibx.plugins.intellij.psi;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiClassType;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiField;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.PsiReference;
import com.intellij.psi.PsiType;
import com.intellij.psi.util.PropertyUtil;
import com.intellij.psi.xml.XmlAttribute;
import com.intellij.psi.xml.XmlAttributeValue;
import com.intellij.psi.xml.XmlTag;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.NotNull;

class FieldReference implements PsiReference {
	private final PsiClass psiClass;
	private final XmlAttribute fieldAttribute;
	private final XmlAttributeValue fieldAttributeValue;
	private final String[] fields;

	public FieldReference(final XmlAttribute fieldAttribute, final PsiClass psiClass) {
		this.psiClass = psiClass;
		this.fieldAttribute = fieldAttribute;
		this.fieldAttributeValue = fieldAttribute.getValueElement();
		fields = PropertyUtil.getReadableProperties(psiClass, true);
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
		PsiField field = findFieldByName(psiClass, value);
		// if we didn't find the field, search for one whose name starts with the value typed
		if (field == null) {
			for (String property : fields) {
				if (property.startsWith(value)) {
					field = findFieldByName(psiClass, property);
				}
			}
		}
		return field;
	}

	public String getCanonicalText() {
		return fieldAttributeValue.getValue();
	}

	public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
		fieldAttribute.setValue(PropertyUtil.getPropertyName(newElementName));
		return fieldAttributeValue;
	}

	public PsiElement bindToElement(@NotNull PsiElement element) throws IncorrectOperationException {
		throw new IncorrectOperationException();
	}

	public boolean isReferenceTo(PsiElement element) {
		return resolve() == element;
	}

	public Object[] getVariants() {
		return fields;
	}

	public boolean isSoft() {
		return false;
	}

	private PsiField findFieldByName(PsiClass psiClass, String fieldName) {
		PsiField field = psiClass.findFieldByName(fieldName, true);
		if (field != null) {
			return field;
		}
		XmlTag tag = fieldAttribute.getParent();
		if (tag == null || tag.getParentTag() == null) {
			return null;
		}
		String getMethod = tag.getParentTag().getAttributeValue("get-method");
		if (getMethod == null) {
			return null;
		}
		PsiMethod[] methods = psiClass.findMethodsByName(getMethod, true);
		if (methods.length < 1) {
			return null;
		}
		PsiType returnType = methods[0].getReturnType();
		if (returnType instanceof PsiClassType) {
			PsiClass clazz = ((PsiClassType) returnType).resolve();
			if (clazz == null) {
				return null;
			}
			return findFieldByName(clazz, fieldName);
		}
		return null;
	}

}
