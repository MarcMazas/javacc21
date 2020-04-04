/* Generated by: JavaCC 21 Parser Generator. Do not edit. CompilationUnit.java */
package com.javacc.parser.tree;

import java.util.*;

import com.javacc.parser.*;
@SuppressWarnings("unused")
public class CompilationUnit extends BaseNode {
    public List<ImportDeclaration>importDeclarations=new ArrayList<ImportDeclaration>();
    public String getPackageName() {
        PackageDeclaration jpd=getPackageDeclaration();
        return jpd==null?null:
        jpd.getPackageName();
    }

    public PackageDeclaration getPackageDeclaration() {
        return firstChildOfType(PackageDeclaration.class);
    }

    public List<ImportDeclaration>getImportDeclarations() {
        return childrenOfType(ImportDeclaration.class);
    }

    public List<TypeDeclaration>getTypeDeclarations() {
        return childrenOfType(TypeDeclaration.class);
    }

    public void addImportDeclaration(ImportDeclaration decl) {
        ListIterator<Node>iterator=iterator();
        Node n=null;
        while (!(n instanceof TypeDeclaration)&&!(n instanceof Annotation)) {
            n=iterator.next();
        }
        if (iterator.hasPrevious()) {
            iterator.previous();
            iterator.add(decl);
        }
        else {
            prepend(decl);
        }
    }

}
