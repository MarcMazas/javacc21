/* Generated by: JavaCC 21 Parser Generator. Do not edit. RegexpSequence.java */
package com.javacc.parser.tree;

import java.util.*;

import com.javacc.lexgen.RegularExpression;
import com.javacc.parser.*;
@SuppressWarnings("unused")
public class RegexpSequence extends RegularExpression {
    public List<RegularExpression>getUnits() {
        return childrenOfType(RegularExpression.class);
    }

}
