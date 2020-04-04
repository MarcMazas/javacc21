/* Generated by: JavaCC 21 Parser Generator. Do not edit. TreeBuildingAnnotation.java */
package com.javacc.parser.tree;

import com.javacc.parser.*;
@SuppressWarnings("unused")
public class TreeBuildingAnnotation extends BaseNode {
    public String getNodeName() {
        String image=null;
        for (Node n : children) {
            if (n instanceof Token) {
                Token t=(Token) n;
                if (t.getId()==JavaCCConstants.HASH_ID) {
                    image=t.toString().substring(1);
                }
            }
        }
        return image;
    }

    public Expression getCondition() {
        return firstChildOfType(Expression.class);
    }

    public boolean getGtNode() {
        for (Node n : children) {
            if (n instanceof Token) {
                Token t=(Token) n;
                if (t.getId()==JavaCCConstants.GT) {
                    return true;
                }
            }
        }
        return false;
    }

    public boolean getVoid() {
        return"void".equals(getNodeName());
    }

    private boolean forced;
    public boolean isForced() {
        return forced;
    }

    public void setForced(boolean forced) {
        this.forced=forced;
    }

}
