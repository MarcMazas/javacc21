/* Generated by: JavaCC 21 Parser Generator. InvalidToken.java */
package com.javacc.parser;

/**
 * Token subclass to represent lexically invalid input
 */
public class InvalidToken extends Token {
    public InvalidToken(String image) {
        super(JavaCCConstants.INVALID,image);
    }

    public InvalidToken() {
        super(JavaCCConstants.INVALID);
    }

    public String getNormalizedText() {
        return"Lexically Invalid Input:"+image;
    }

    public boolean isDirty() {
        return true;
    }

}
