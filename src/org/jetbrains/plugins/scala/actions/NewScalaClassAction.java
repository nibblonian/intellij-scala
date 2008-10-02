package org.jetbrains.plugins.scala.actions;

import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.plugins.scala.ScalaBundle;
import org.jetbrains.plugins.scala.icons.Icons;
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile;
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition;

/**
 * @author ilyas
 */
public class NewScalaClassAction extends NewScalaActionBase {

  public NewScalaClassAction() {
    super(ScalaBundle.message("newclass.menu.action.text"),
        ScalaBundle.message("newclass.menu.action.description"),
        Icons.CLASS);
  }

  protected String getActionName(PsiDirectory directory, String newName) {
    return ScalaBundle.message("newclass.menu.action.text");
  }

  protected String getDialogPrompt() {
    return ScalaBundle.message("newclass.dlg.prompt");
  }

  protected String getDialogTitle() {
    return ScalaBundle.message("newclass.dlg.title");
  }

  protected String getCommandName() {
    return ScalaBundle.message("newclass.command.name");
  }

  @NotNull
  protected PsiElement[] doCreate(String newName, PsiDirectory directory) throws Exception {
    PsiFile file = createClassFromTemplate(directory, newName, "ScalaClass.scala");
    if (file instanceof ScalaFile) {
      ScalaFile scalaFile = (ScalaFile) file;
      PsiClass[] classes = scalaFile.getClasses();
      if (classes.length == 1 && classes[0] instanceof ScTypeDefinition) {
        ScTypeDefinition definition = (ScTypeDefinition) classes[0];
        PsiElement eBlock = definition.extendsBlock();
        return eBlock != null ? new PsiElement[]{definition, eBlock} : new PsiElement[]{definition};
      }
    }
    return new PsiElement[]{file};
  }
}
