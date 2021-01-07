[#ftl strict_vars=true]
[#--
/* Copyright (c) 2008-2019 Jonathan Revusky, revusky@javacc.com
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright notices,
 *       this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name Jonathan Revusky, Sun Microsystems, Inc.
 *       nor the names of any contributors may be used to endorse 
 *       or promote products derived from this software without specific prior written 
 *       permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */
 --]
/* Generated by: ${generated_by}. ${filename} */
[#if grammar.parserPackage?has_content]
package ${grammar.parserPackage};
[/#if]
import java.util.*;
import java.lang.reflect.*;
import java.util.function.Predicate;
[#if grammar.settings.FREEMARKER_NODES?? && grammar.settings.FREEMARKER_NODES]
import freemarker.template.*;
[/#if]

public interface Node extends Comparable<Node> 
[#if grammar.settings.FREEMARKER_NODES?? && grammar.settings.FREEMARKER_NODES]
   , TemplateNodeModel, TemplateScalarModel
[/#if] {

    /** Life-cycle hook method called after the node has been made the current
	 *  node 
	 */
    void open();

  	/** 
  	 * Life-cycle hook method called after all the child nodes have been
     * added. 
     */
    void close();


    /**
     * The input source (usually a filename) from which this Node came from
     */
    String getInputSource();

    /**
     * Set the input source, typically only used internally
     */
    void setInputSource(String name);

    
    /**
     * Returns whether this node has any children.
     * 
     * @return Returns <code>true</code> if this node has any children,
     *         <code>false</code> otherwise.
     */
    default boolean hasChildNodes() {
       return getChildCount() > 0;
    }

    void setParent(Node n);
     
    Node getParent();
     
     // The following 9 methods will typically just 
     // delegate straightforwardly to a List object that
     // holds the child nodes
     
     void addChild(Node n);
     
     void addChild(int i, Node n);

     Node getChild(int i);

     void setChild(int i, Node n);
     
     Node removeChild(int i);
     
     boolean removeChild(Node n);
     
     default int indexOf(Node child) {
         for (int i=0; i<getChildCount(); i++) {
             if (child == getChild(i)) {
                 return i;
             }
         }
         return -1;
     }
     /**
      * Used to order Nodes by location.
      */
     default int compareTo(Node n) {
         if (this == n) return 0;
         int diff = this.getBeginLine() - n.getBeginLine();
         if (diff !=0) return diff;
         diff = this.getBeginColumn() -n.getBeginColumn();
         if (diff != 0) return diff;
         // A child node is considered to come after its parent.
         diff = n.getEndLine() - this.getEndLine();
         if (diff != 0) return diff;
         return n.getEndColumn() - this.getEndColumn();
     }
     
     void clearChildren();
       
     int getChildCount();
     
     /**
      * Most implementations of this should return a copy or
      * an immutable wrapper around the list.
      */
      default List<Node> children() {
         List<Node> result = new ArrayList<>();
         for (int i = 0; i < getChildCount(); i++) {
             result.add(getChild(i));
         }
         return result;
      }

     // The following 3 methods will typically delegate
     // straightforwardly to a Map<String, Object> object-s get/set/containsKey/keySet methods.
          
     Object getAttribute(String name);
     
     void setAttribute(String name, Object value);
     
     boolean hasAttribute(String name);
     
     java.util.Set<String> getAttributeNames();


    /**
     * @return a List containing all the tokens in a Node
     * @param includeCommentTokens Whether to include comment tokens
     */
     default List<Token> getAllTokens(boolean includeCommentTokens) {
		List<Token> result = new ArrayList<Token>();
        for (Iterator<Node> it = iterator(); it.hasNext();) {
            Node child = it.next();
            if (child instanceof Token) {
                Token token = (Token) child;
                if (token.isUnparsed()) {
                    continue;
                }
                if (includeCommentTokens) {
                    ArrayList<Token> comments = null;
                    Token prev = token.getPreviousToken();
                    while (prev != null && prev.isUnparsed()) {
                        if (comments == null) comments = new ArrayList<>();
                        comments.add(prev);
                        prev = prev.getPreviousToken();
                    }
                    if (comments !=null) {
                        Collections.reverse(comments);
                        result.addAll(comments);
                    }
                }
                result.add(token);
            } 
            else if (child.getChildCount() >0) {
               result.addAll(child.getAllTokens(includeCommentTokens));
            }
        }
        return result;
    }

    /**
     * @return All the tokens in the node that 
     * are "real" (i.e. participate in parsing)
     */
    default List<Token> getRealTokens() {
        return descendants(Token.class, t->!t.isUnparsed());
    }
    
[#if !grammar.hugeFileSupport && !grammar.userDefinedLexer]
     default FileLineMap getFileLineMap() {
         return FileLineMap.getFileLineMapByName(getInputSource());
     }

     default String getSource() {
        return getFileLineMap().getText(getBeginLine(), getBeginColumn(), getEndLine(), getEndColumn());
    }
[/#if]          
      
     int getBeginLine();
     
     int getEndLine();
     
     int getBeginColumn();
     
     int getEndColumn();
     
     void setBeginLine(int beginLine);
     
     void setEndLine(int endLine);
     
     void setBeginColumn(int beginColumn);
     
     void setEndColumn(int endColumn);
     
     default String getLocation() {
         //return "line " + getBeginLine() + ", column " + getBeginColumn() + " of " + getInputSource();
         return getInputSource() + ":" + getBeginLine() + ":" + getBeginColumn();
     }
     
     
     /**
      *  A regular node was created by the regular operations of the parsing machinery
      * applying the rules, consuming tokens and building up the tree.
      * An unparsed node is typically created as part of error recovery or possibly
      * some post-parsing tree-walking adjustments maybe.
      */
     default boolean isUnparsed() {
        return false;
     }
     
     void setUnparsed(boolean b);
     
    default <T extends Node>T firstChildOfType(Class<T>clazz) {
        for (Node child : children()) {
            if (clazz.isInstance(child)) {
                return clazz.cast(child);
            }
        }
        return null; 
     }

     default <T extends Node>T firstDescendantOfType(Class<T> clazz) {
         for (Node child : children()) {
             if (clazz.isInstance(child)) return clazz.cast(child);
             else return child.firstDescendantOfType(clazz);
         }
         return null;
     }

    default <T extends Node>List<T>childrenOfType(Class<T>clazz) {
        List<T>result=new java.util.ArrayList<>();
        for (Node child : children()) {
            if (clazz.isInstance(child)) {
                result.add(clazz.cast(child));
            }
        }
        return result;
   }
   
   default <T extends Node> List<T> descendantsOfType(Class<T> clazz) {
        List<T> result = new ArrayList<T>();
        for (Node child : children()) {
            if (clazz.isInstance(child)) {
                result.add(clazz.cast(child));
            } 
            result.addAll(child.descendantsOfType(clazz));
        }
        return result;
   }
   
   default <T extends Node> T firstAncestorOfType(Class<T> clazz) {
        Node parent = this;
        while (parent !=null) {
           parent = parent.getParent();
           if (clazz.isInstance(parent)) {
               return clazz.cast(parent);
           }
        }
        return null;
    }

[#if grammar.tokensAreNodes]
    /**
     * return the very first token that is part of this node
     * may be an unparsed (i.e. special) token.
     */
    default Token getFirstToken() {
        Node first = getFirstChild();
        if (first == null) return null;
        if (first instanceof Token) {
            Token tok = (Token) first;
            while (tok.getPreviousToken() != null && tok.getPreviousToken().isUnparsed()) {
                tok = tok.getPreviousToken();

            }
           return tok;
        }
        return first.getFirstToken(); 
    }

    default Token getLastToken() {
        Node last = getLastChild();
        if (last == null) return null;
        if (last instanceof Token) {
            return (Token) last;
        }
        return last.getLastToken();
    }
[/#if]    

    default Node findNodeAt(int line, int column) {
        if (!isIncluded(line, column)) {
            return null;
        }
        for (Node child : children()) {
            Node match = child.findNodeAt(line, column);
            if (match != null) {
                return match;
            }
        }
        return this;
    }

    default void copyLocationInfo(Node to) {
        if (getInputSource()!=null && to.getInputSource()==null) {
            to.setInputSource(getInputSource()); //REVISIT
        }
        to.setBeginLine(this.getBeginLine());
        to.setBeginColumn(this.getBeginColumn());
        to.setEndLine(this.getEndLine());
        to.setEndColumn(this.getEndColumn());
    }

    default void replace(Node toBeReplaced) {
        toBeReplaced.copyLocationInfo(this);
        Node parent = toBeReplaced.getParent();
        if (parent !=null) {
           int index = parent.indexOf(toBeReplaced);
           parent.setChild(index, this);
        }
    }
    
    /**
     * Returns true if the given position (line,column) is included in the given
     * node and false otherwise.
     * 
     * @param line   the line position
     * @param column the column position
     * @return true if the given position (line,column) is included in the given
     *         node and false otherwise.
     */
    default boolean isIncluded(int line, int column) {
        return isIncluded(getBeginLine(), getBeginColumn(),getEndLine(), getEndColumn(), line,
                column);
    }

    default boolean isIncluded(int beginLine, int beginColumn, int endLine, int endColumn, int line,
            int column) {
        if (beginLine == line && beginColumn == column) {
            return true;
        }
        if (endLine == line && endColumn == column) {
            return true;
        }            
        return !isAfter(beginLine, beginColumn, line, column) && isAfter(endLine, endColumn, line, column);
    }
    
    /**
     * Returns the first child of this node. If there is no such node, this returns
     * <code>null</code>.
     * 
     * @return the first child of this node. If there is no such node, this returns
     *         <code>null</code>.
     */
    default Node getFirstChild() {
        return getChildCount() > 0 ? getChild(0) : null;
    }
    
    
     /**
     * Returns the last child of the given node. If there is no such node, this
     * returns <code>null</code>.
     * 
     * @return the last child of the given node. If there is no such node, this
     *         returns <code>null</code>.
     */ 
    default Node getLastChild() {
        int count = getChildCount();
        return count>0 ? getChild(count-1): null;
    }


    static boolean isAfter(int line1,int column1,int line2,int column2) {
        if (line1>line2) {
            return true;
        }
        if (line1==line2) {
            return column1>=column2;
        }
        return false;
    }
    
      
    default Node getRoot() {
        Node parent = this;
        while (parent.getParent() != null ) {
            parent = parent.getParent();
        }
        return parent; 
    }
    
     static public List<Token> getTokens(Node node) {
        List<Token> result = new ArrayList<Token>();
        for (Node child : node.children()) {
            if (child instanceof Token) {
                result.add((Token) child);
            } else {
                result.addAll(getTokens(child));
            }
        }
        return result;
    }
        
        
    static public List<Token> getRealTokens(Node n) {
        List<Token> result = new ArrayList<Token>();
		for (Token token : getTokens(n)) {
		    if (!token.isUnparsed()) {
		        result.add(token);
		    }
		}
	    return result;
    }

    default List<Node> descendants(Predicate<Node> predicate) {
        return descendants(Node.class, predicate);
    }

    default <T extends Node> List<T> descendants(Class<T> clazz) {
        return descendants(clazz, null);
    }

    default <T extends Node> List<T> descendants(Class<T> clazz, Predicate<T> predicate) {
       List<T> result = new ArrayList<>();
       for (Node child : children()) {
          if (clazz.isInstance(child)) {
              T t = clazz.cast(child);
              if (predicate == null || predicate.test(t)) {
                  result.add(t);
              }
          }
          result.addAll(child.descendants(clazz, predicate)); 
       }
       return result;
    }

    default void dump(String prefix) {
        String output = (this instanceof Token) ? toString().trim() : getClass().getSimpleName();
        if (output.length() >0) {
            System.out.println(prefix + output);
        }
        for (Iterator<Node> it = iterator(); it.hasNext();) {
            Node child = it.next();
            child.dump(prefix+"  ");
        }
    }

    default void dump() {
        dump("");
    }

    // NB: This is not thread-safe
    // If the node's children could change out from under you,
    // you could have a problem.

    default public ListIterator<Node> iterator() {
        return new ListIterator<Node>() {
            private int current = -1;
            private boolean justModified;
            
            public boolean hasNext() {
                return current+1 < getChildCount();
            }
            
            public Node next() {
                justModified = false;
                return getChild(++current);
            }
            
            public Node previous() {
                justModified = false;
                return getChild(--current);
            }
            
            public void remove() {
                if (justModified) throw new IllegalStateException();
                removeChild(current);
                --current;
                justModified = true;
            }
            
            public void add(Node n) {
                if (justModified) throw new IllegalStateException();
                addChild(current+1, n);
                justModified = true;
            }
            
            public boolean hasPrevious() {
                return current >0;
            }
            
            public int nextIndex() {
                return current + 1;
            }
            
            public int previousIndex() {
                return current;
            }
            
            public void set(Node n) {
                setChild(current, n);
            }
        };
    }

 	static abstract public class Visitor {
		
		static private Method baseVisitMethod;
		private HashMap<Class<? extends Node>, Method> methodCache = new HashMap<>();
		
		static private Method getBaseVisitMethod() throws NoSuchMethodException {
			if (baseVisitMethod == null) {
				baseVisitMethod = Node.Visitor.class.getMethod("visit", new Class[] {Node.class});
			} 
			return baseVisitMethod;
		}
		
		private Method getVisitMethod(Node node) {
			Class<? extends Node> nodeClass = node.getClass();
			if (!methodCache.containsKey(nodeClass)) {
				try {
					Method method = this.getClass().getMethod("visit", new Class[] {nodeClass});
					if (method.equals(getBaseVisitMethod())) {
						method = null; // Have to avoid infinite recursion, no?
					}
					methodCache.put(nodeClass, method);
				}
				catch (NoSuchMethodException nsme) {
					methodCache.put(nodeClass, null);
				}
			}
	        return methodCache.get(nodeClass);
		}
		
		/**
		 * Tries to invoke (via reflection) the appropriate visit(...) method
		 * defined in a subclass. If there is none, it just calls the fallback() routine. 
		 */
		public final void visit(Node node) {
			Method visitMethod = getVisitMethod(node);
			if (visitMethod == null) {
				fallback(node);
			} else try {
				visitMethod.invoke(this, new Object[] {node});
			} catch (InvocationTargetException ite) {
	    		Throwable cause = ite.getCause();
	    		if (cause instanceof RuntimeException) {
	    			throw (RuntimeException) cause;
	    		}
	    		throw new RuntimeException(ite);
	 		} catch (IllegalAccessException iae) {
	 			throw new RuntimeException(iae);
	 		}
		}
		
		public final void recurse(Node node) {
            for (Node child : node.children()) {
                visit(child);
            }
		}
		
		/**
		 * If there is no specific method to visit this node type,
		 * it just uses this method. The default base implementation
		 * is just to recurse over the nodes.
		 */
		public void fallback(Node node) {
		    recurse(node);
		}
    }
}
