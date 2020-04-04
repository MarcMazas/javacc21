/* Generated by: JavaCC 21 Parser Generator. Do not edit. TypeDeclaration.java */
package com.javacc.parser.tree;

import java.util.*;

import com.javacc.parser.*;
@SuppressWarnings("unused")
public class TypeDeclaration extends BaseNode {
    public String getName() {
        for (Node n : children) {
            if (n instanceof Identifier) {
                return n.toString();
            }
        }
        throw new RuntimeException("Should never get here.");
    }

    public boolean getInterface() {
        for (Node n : children) {
            if (n instanceof Token) {
                Token t=(Token) n;
                if (t.getId()==INTERFACE) {
                    return true;
                }
            }
        }
        return false;
    }

    public String getPackageName() {
        String packageName=null;
        if (parent instanceof CompilationUnit) {
            CompilationUnit jcu=(CompilationUnit) parent;
            packageName=jcu.getPackageName();
        }
        return packageName;
    }

    public String getFullName() {
        String name=getName();
        String packageName=getPackageName();
        if (packageName!=null&&packageName.length()>0) {
            return packageName+"."+name;
        }
        return name;
    }

    public TypeParameterList getTypeParameterList() {
        ListIterator<Node>iterator=iterator();
        Node n=iterator.next();
        while (n instanceof Identifier) {
            n=iterator.next();
        }
        n=iterator.next();
        if (n instanceof TypeParameterList) {
            return(TypeParameterList) n;
        }
        return null;
    }

    public ClassOrInterfaceBody getBody() {
        return firstChildOfType(ClassOrInterfaceBody.class);
    }

    public ExtendsList getExtendsList() {
        return firstChildOfType(ExtendsList.class);
    }

    public ImplementsList getImplementsList() {
        return firstChildOfType(ImplementsList.class);
    }

    public CompilationUnit getCompilationUnit() {
        Node parent=getParent();
        if (parent instanceof CompilationUnit) {
            return(CompilationUnit) parent;
        }
        return null;
    }

    public List<ImportDeclaration>getImportDeclarations() {
        CompilationUnit jcu=getCompilationUnit();
        if (jcu==null) {
            return new ArrayList<ImportDeclaration>();
        }
        return jcu.getImportDeclarations();
    }

    public void addElements(List<ClassOrInterfaceBodyDeclaration>elements) {
        Set<String>keys=new HashSet<String>();
        for (ClassOrInterfaceBodyDeclaration decl : elements) {
            String key=decl.getFullNameSignatureIfMethod();
            if (key!=null) {
                keys.add(key);
            }
        }
        for (Iterator<Node>it=getBody().iterator(); it.hasNext(); ) {
            Node n=it.next();
            if (n instanceof ClassOrInterfaceBodyDeclaration) {
                String s=((ClassOrInterfaceBodyDeclaration) n).getFullNameSignatureIfMethod();
                if (keys.contains(s)) {
                    it.remove();
                }
            }
        }
        getBody().prepend(elements);
    }

    public void addAnnotations(Set<Annotation>annotations) {
        Node parent=this.getParent();
        int index=parent.indexOf(this);
        for (Annotation annotation : annotations) {
            parent.addChild(index,annotation);
        }
    }

    public boolean isClass() {
        for (Node n : children) {
            if (n instanceof Token) {
                Token t=(Token) n;
                if (t.getId()==CLASS) {
                    return true;
                }
            }
        }
        return false;
    }

    public void addImplements(ObjectType type) {
        ImplementsList implementsList=getImplementsList();
        if (implementsList==null) {
            implementsList=new ImplementsList();
            ListIterator<Node>iterator=iterator();
            while (iterator.hasNext()) {
                Node node=iterator.next();
                if (node instanceof ClassOrInterfaceBody||node instanceof EnumBody) break;
            }
            iterator.previous();
            iterator.add(implementsList);
        }
        implementsList.addType(type);
    }

    public void addExtends(ObjectType type) {
        ExtendsList extendsList=getExtendsList();
        if (extendsList==null) {
            extendsList=new ExtendsList();
            ListIterator<Node>iterator=iterator();
            while (iterator.hasNext()) {
                Node node=iterator.next();
                if (node instanceof ImplementsList||node instanceof ClassOrInterfaceBody) {
                    break;
                }
            }
            iterator.previous();
            iterator.add(Token.newToken(WHITESPACE," "));
            iterator.next();
            iterator.add(extendsList);
        }
        extendsList.addType(type,getInterface());
    }

}
