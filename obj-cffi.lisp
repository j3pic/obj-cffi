(defpackage #:obj-cffi
  (:use #:common-lisp #:cffi))

(in-package #:obj-cffi)

;; An OSX app in plain C: https://github.com/jimon/osx_app_in_plain_c
;;
;; It uses the COCOA API.

;;
;; The NEXT browser's upcoming GUI toolkit may obviate the need for calling
;; Objective-C from Lisp.

(defparameter *test-lib* (load-foreign-library #P"/Users/jeremyphelps/Library/Developer/Xcode/DerivedData/Testing-gteieshxlukrkbdowshxnxgffxkl/Build/Products/Debug/libTesting.dylib"))

;; In Objective-C, exception handling is implemented with the C functions
;; obj_exception_throw, obj_exception_try_enter, obj_exception_try_exit, and
;; others. The full list of functions can be found by searching for the text
;; string "objc_exception" with CScope in the objc4 library.
;;
;; The following struct is declared:
;;
;; typedef struct {
;;    int version;
;;    void (*throw_exc)(id);      // version 0
;;    void (*try_enter)(void *);  // version 0
;;    void (*try_exit)(void *);   // version 0
;;    id   (*extract)(void *);    // version 0
;;    int (*match)(Class, id);    // version 0
;; } objc_exception_functions_t;
;;
;; There's a static variable that contains the current exception table.
;;
;; objc4 provides a C interface to exception handling, but it dispatches
;; some thing to the CoreFoundation.
;;
;; I found a functoin named findHandler that has a comment saying it
;; walks the stack to look for a frame with an objc catch handler. Direct stack
;; inspection is the absolute worst thing that could possibly happen. It looks
;; like my dream of working with Objective-C system libraries without writing
;; any custom Objective-C dependencies cannot be realized. But there may still
;; be hope if there's a function to add an exception handler to the stack. But
;; then again, that function may end up simply corrupting the Lisp stack if it
;; doesn't turn out to be a C macro.
;;
;; There is a function called objc_addExceptionHandler, which expects there to be
;; an enclosing frame with another kind of handler called a "catch handler". Perhaps
;; a catch handler is installed by objc_exception_try_enter(). But that's hard to
;; see because that function delegates all of its functionality to a function
;; pointer, and the code is probably provided by CoreFoundation.
;;
;; Apple used to have a stub of documentation for OBJC exceptions. They removed
;; it, and they did not allow the Wayback Machine to archive it.
;;
;; Some Node.js guys tried to do ObjC exception handling from JavaScript:
;;
;; https://github.com/node-ffi/node-ffi/issues/18
;;
;; This includes a Gist that attempts to implement the functionality,
;; but fails.
;;
;; Someone reported that these functions are no longer "exported" by
;; the current Obj-C runtime.
;;
;; They finally settled on a solution that involves writing custom Objective-C
;; code, exactly what I'm trying to avoid doing, since Lisp libraries that
;; require compilation of bits of Objective-C do not actually work.
;; The required Objective-C function would be tiny: It would invoke a C
;; callback and then return either an exception object or NIL. The callback,
;; which could be a Lisp function defined with DEFCALLBACK, would have to
;; use a side-channel to return the actual return value of the function.
;; All of this could be wrapped in macros.
;;
;; According to StackOverflow, Apple promised to document Objective-C 2.0's
;; ABI, but never did so. The only way to fully understand it is to analyze
;; both obj4 and clang: https://stackoverflow.com/questions/8461411/objective-c-2-0-abi-specification
;; 
;; It seems that Objective-C access is inextricably chained to the XCode IDE.

;; Even worse: There's a Stack Overflow answer https://stackoverflow.com/questions/4648952/objective-c-exceptions
;; that states that COCOA doesn't handle exceptions at all (not even using @try / @finally!), so if you throw
;; an exception from a callback, it can leave COCOA in an undefined state.
;;
;; That means exceptions can't be safely thrown from Lisp callbacks to COCOA, which is an unmitigated disaster.
;; Worse than exceptions are restarts! The best possible solution to this problem would be some kind of bridge
;; that translates nonlocal control transfers into regular chains of returns... but that would inevitably break
;; some things. For example, suppose we had this:
;;
;;   (stubborn-cocoa-function-that-only-returns-when-it-feels-like-it
;;     (lambda (foo)
;;       (when we-want-to-get-out-of-this
;;         (throw :some-tag an-arbitrary-value))))
;;
;; Suppose we somehow translate the THROW into a normal return. What if
;; Apple thought that they knew better than you whether their stubborn
;; function should pay attention to your callback's return code? Then
;; there may be no way to get out of this that wouldn't break COCOA.
;;
;; So Apple's exception-phobia must infect Lisp code extensively.
;;
;; One way to make Lisp exceptions safe is to ensure that Lisp callbacks
;; are never called in the same stack as regular Lisp code. The main thread
;; should be entirely controlled by COCOA, so that no restarts or CATCH
;; tags are defined in the dynamic extent under which any Lisp exception
;; or THROW may occur.
;;
;; This way, an uncaught Lisp exception will invoke the debugger without
;; unwinding the stack.
;;
;; But it's still possible for one Lisp callback to be in the dynamic extent
;; of another one, with COCOA frames in between. Then the second Lisp callback
;; can throw an exception to the first one, unwinding the COCOA frames with
;; unpredictable results.
;;
;; It just occurred to me that this might be a problem that affects other
;; Lisp bindings to GUI libraries. So I looked at CL-CFFI-GTK, but the tutorial
;; programs don't have any examples where there is are two Lisp functions with
;; GTK stack frames in between them. This library puts the GTK mainloop in
;; a separate thread, and doesn't provide any restarts or tags to jump to.
;; Then, GTK's mainloop only calls one Lisp callback at any given time.
;;
;; Calls that might get another callback going elsewhere on the stack will
;; instead insert a message into GTK's message queue, and the mainloop
;; will call the second Lisp callback in a stack that does not also
;; contain the first one.
;;
;; In this implementation, callbacks can throw exceptions, but doing
;; so doesn't do anything useful, so in practice callbacks will catch
;; all their exceptions or refrain from throwing them.
;;
;; I've written code in Objective-C that throws exceptions across
;; Objective-C frames. I noticed nothing out of the ordinary, but the
;; corruption was probably there.
;;
;; If I write a McCLIM backend for COCOA and the McCLIM callbacks
;; execute within the real dynamic extent of the COCOA callbacks,
;; then exception phobia will spread to CLIM code, which is absolutely
;; unacceptable.
;;
;; One possible solution to this is to set up a marionette model: The
;; actual COCOA callbacks would run in a separate thread, and they'd
;; use a queue to talk to another thread in which CLIM callbacks
;; can be called. The CLIM callbacks would have access to an API that
;; moves Objective-C calls back to the COCOA thread by sending closures
;; through a return queue.
;;
;; But even this comes with certain difficulties. If an exception
;; is thrown from CLIM, it would be in an entirely separate stack
;; from the COCOA thread. Throwing from CLIM wouldn't unwind the
;; COCOA stack, which means it might get into a state that can't
;; be recovered from.

(defcfun "throw_exception" :void)

;; Foreign type OBJC-CLASS

(define-foreign-type objc-class ()
  ((foreign-pointer :initform nil :initarg :foreign-pointer))
  (:actual-type :pointer))

(define-parse-method objc-class ()
  (make-instance 'objc-class))

(defmethod translate-to-foreign (class (type objc-class))
  (slot-value class 'foreign-pointer))

(defmethod translate-from-foreign (pointer (type objc-class))
  (make-instance 'objc-class :foreign-pointer pointer))

;; END OBJC-CLASS. It should now be possible to make OBJC foreign functions
;; that use CLOS objects as Objective-C classes.
;;
;; TODO: Make OBJC-CLASS inherit from STANDARD-CLASS so you can make CLOS
;;       classes that inherit from it.
;;
;; TODO: Figure out if the MOP can be used to make it so that defining a CLOS
;;       class that subclasses OBJC-CLASS also defines an Objective-C version
;;       of the class. CLOS's linearization of the class hierarchy could possibly
;;       even make it possible to port classes with multiple inheritance to Objective-C.

(defcfun "class_getName" :string
  (class :pointer))

(defcfun "class_getSuperclass" :pointer
  (class :pointer))

(defcfun ("objc_allocateClassPair" objc-alloc-class-pair) (:pointer)
  (superclass (:pointer))
  (name :string)
  (extra-byte #+64-bit :uint64
	      #-64-bit :uint3))

(defcfun ("class_addIvar" class-add-ivar) :int
  (cls :pointer)
  (name :string)
  (size #+x86-64 :unsigned-long-long
	#-x86-64 :ulong) ;; I really have no idea what size_t will be whem MacOS moves to ARM processors.
  (alignment :unsigned-char)
  (types :string))

(defcfun ("class_addMethod" class-add-method) :int
  (class :pointer)
  (selector :pointer)
  (imp :pointer)
  (type-string :string))

(defcfun ("object_setIvar" object-set-ivar) :void
  (obj :pointer)
  (ivar :pointer)
  (value :pointer))

(defcfun ("object_setInstanceVariable" object-set-instance-variable) :pointer
  (obj :pointer)
  (name :string)
  (value :pointer))

;; FIXME: I'm trying to implement a class using this working example:
;;
;;        https://gist.github.com/mikeash/7603035
;;
;;        The author uses a function called imp_implementationWithBlock, which creates an
;;        IMP object from an Objective-C block.
;;
;;        This seemed like an insurmountable obstacle, until I noticed that IMP objects
;;        are just functions.
;;
;; In <objc.h>, the following definition of the IMP type exists:
;;
;;     typedef id _Nullable (*IMP)(id _Nonnull, SEL _Nonnull, ...);
;;
;; So perhaps IMPs can simply be a CFFI callback (use CFFI:DEFCALLBACK)
;;
;;  Upon looking at the macroexpansion for this, it looks like CFFI does not
;;  support using LAMBDAs as callbacks, although it uses SB-ALIEN::ALIEN-LAMBDA in
;;  its SBCL implementation.

(defmacro defimp (name arguments &body body)
  "Define an Objective-C IMP function. The ARGUMENTS are just a list of variable names. When
your IMP function is called, these arguments will all be CFFI :POINTERs.

The function is expected to return an Objective-C 'id' object. The generated CFFI callback
will be declared to return the type :POINTER.

The function will have an implicit first argument declared as (SELF :POINTER)."
  `(defcallback ,name :pointer ((self :pointer) ,@(loop for arg in arguments
						     collect `(,arg :pointer))
				,@body)))

(defcfun "objc_lookUpClass" (:pointer)
  (name :string))

(defcfun "class_isMetaClass" :int
  (class :pointer))

(defcfun "class_getInstanceSize" :uint64
  (class :pointer))

(defcfun "class_copyMethodList" :pointer
  (class :pointer)
  (out-count (:pointer :uint)))

(defcfun "class_copyIvarList" :pointer
  (class :pointer)
  (out-count (:pointer :uint)))

(defcfun ("class_getInstanceVariable" class-get-instance-variable) :pointer
  (class :pointer)
  (name :string))

(defcfun ("ivar_getOffset" ivar-get-offset) #+x86-64 :long-long #-x86-64 :long
  (ivar :pointer))

(defcfun "method_getName" :pointer
  (method :pointer))

(defcfun "sel_getName" :string
  (selector :pointer))

(defcfun ("sel_registerName" sel-register-name) :pointer
  (string :string))

(defcfun "method_copyReturnType" :pointer
  (method :pointer))

(defcfun ("method_copyReturnType" method-return-type-cstring) :string
  (method :pointer))

(defcstruct objc-method-description
  (selector :pointer)
  (types :string))

(defcfun "method_getDescription" (:pointer (:struct objc-method-description))
  (method :pointer))

;;      SEL _Nullable name;               /**< The name of the method */
;;    char * _Nullable types;           /**< The types of the method arguments */




;; (defcstruct objc-super ...)
;; TODO: https://developer.apple.com/documentation/objectivec/objc_super?language=objc

;; Sending messages

(defmacro define-msg-send-functions ()
  `(progn
     ,@(loop for return-type in '(:int :uint :long :ulong #-(or x86-64 x86) :float #-(or x86-64 x86) :double
				  :long-long :unsigned-long-long :string
				  :pointer)
	  collect
	    `(defcfun ("objc_msgSend" ,(intern (format nil "OBJC-MSG-SEND-~A" return-type))) ,return-type
	       (self :pointer)
	       (selector :pointer)
	       &rest)
	  collect
	    `(defcfun ("objc_msgSendSuper" ,(intern (format nil "OBJC-MSG-SEND-SUPER~A" return-type))) ,return-type
	       (super :pointer) ;; TODO: Specify it's a pointer to OBJC-SUPER
	       (selector :pointer)
	       &rest))
	
     (defcfun ("objc_msgSend_stret" objc-msg-send-stret) :void
       (struct-addr :pointer)
       (self :pointer)
       (selector :pointer)
       &rest)

     (defcfun ("objc_msgSendSuper_stret" objc-msg-send-super-stret) :void
       (struct-addr :pointer)
       (self :pointer)
       (selector :pointer)
       &rest)
  
     #+(or x86 x86-64)
     ,@`((defcfun ("objc_msgSend_fpret" objc-msg-send-float) :float
	   (self :pointer)
	   (selector :pointer)
	   &rest)

	 (defcfun ("objc_msgSend_fpret" objc-msg-send-double) :double
	   (self :pointer)
	   (selector :pointer)
	   &rest)

	 (defcfun ("objc_msgSendSuper_fpret" objc-msg-send-super-float) :float
	   (self :pointer)
	   (selector :pointer)
	   &rest)

	 (defcfun ("objc_msgSendSuper_fpret" objc-msg-send-super-double) :double
	   (self :pointer)
	   (selector :pointer)
	   &rest))))

(define-msg-send-functions)

;;

(defmacro %send (obj selector-object return-type args &key super)
  (let ((function-name (if (symbolp return-type)
			   (intern (format nil "OBJC-MSG-SEND~A-~A"
					   (if super "-SUPER" "")
					   return-type))
			   (intern (format nil "OBJC-MSG-SEND~A-STRET"
					   (if super "-SUPER" ""))))))
    `(,function-name ,obj ,selector-object ,@args)))

(defcfun "free" :void
  (ptr :pointer))

(defun method-name (method)
 ;; FIXME: The string we get from this may need to be freed.
  (sel-getname (method-getname method)))

(defun c-string->lisp-string (ptr &key free)
  "Given a CFFI pointer, extract a string from it."
  (prog1
      (unless (null-pointer-p ptr)
	(coerce
	 (loop for ix from 0
	    for code = (mem-aref ptr :uint8 ix)
	    until (= code 0)
	    collect (code-char code))
	 'string))
    (when free
      (free ptr))))


(defun method-return-type (method)
  "See the following URL for what little documentation exists about the encoding of
the string returned by this function:

https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/ObjCRuntimeGuide/Articles/ocrtTypeEncodings.html

There are methods that have return types that are not well described by this document. For example:

  (method-return-type (get-method-by-name *ns-string* \"CAMLType\"))

In fact, that method does not have a corresponding entry in any header file so you wouldn't
be able to call it from Objective-C. And, it's totally undocumented."

  (c-string->lisp-string (method-copyreturntype method)
			 :free t))

(defun get-meta-list (class accessor)
  (let ((thing-array (funcall accessor class (null-pointer))))
    (unless (null-pointer-p thing-array)
      (loop
	 for ix from 0
	 for value = (mem-aref thing-array :pointer ix)
	 until (null-pointer-p value)
	 collect value))))
  
(defun get-methods (class)
  "Given a an Objective-C CLASS, returns a list of pointers to Objective-C Method objects."
  (get-meta-list class #'class-copymethodlist))

(defun get-ivars (class)
  "Given an Objective-C CLASS, returns a list of pointers to Objective-C Ivar objects."
  (get-meta-list class #'class-copyivarlist))

(defun get-method-by-name (class name)
  (loop for method in (get-methods class)
     when (string= name (method-name method))
       return method))

(defun methods-matching-regex (class regex)
  (loop for method in (get-methods class)
     when (cl-ppcre:all-matches regex (method-name method))
       collect method))

;; I'm telling CFFI that these are pointers, but they are
;; not.

(defcvar ("OBJC_CLASS_$_NSObject" *ns-object-real*) :pointer)
(defcvar ("OBJC_METACLASS_$_NSObject" *ns-object-meta-real*) :pointer)
(defcvar ("OBJC_IVAR_$_NSObject.isa" *ns-object-ivars-real*) :pointer)


(defvar *ns-object* (objc-lookupclass "NSObject"))
(defvar *objc-nil* (null-pointer))
(defvar *ns-object-meta* (get-var-pointer '*ns-object-meta-real*))
(defvar *ns-object-ivars* (get-var-pointer '*ns-object-ivars-real*))
(defvar *ns-string* (objc-lookupclass "NSString"))

(defparameter *my-class* (objc-alloc-class-pair *ns-object* "MadeWithLisp" 0))
;; The extra-bytes param does not affect the instance size of a class.
;;
;; These classes have an instance size of 8. But a class I created with Xcode
;; ended up with a class size of 16, due to having an instance variable.
;;
;; So how do we create classes with instance variables?
(defparameter *my-big-class* (objc-alloc-class-pair *ns-object* "BigMadeWithLisp" 8))

(defun encode-type (type)
  "Returns a Lisp string encoding the given TYPE."
  (ecase type
    (:id "@")
    ((:sel :selector) ":")
    (:ulong "L")
    (:char "c")
    (:int "i")
    (:short "s")
    (:long "l")
    (:long-long "q")
    (:unsigned-char "C")
    (:uint "I")
    (:ushort "S")
    (:unsigned-long-long "Q")
    (:float "f")
    (:double "d")
    (:bool "B")
    (:void "v")
    (:string "*")
    (:class "#")))
    
;; https://gist.github.com/mikeash/7603035

#| (defun make-test-class ()
  (let* ((c (objc-alloc-class-pair *ns-object* "Test-Person" 0))
	 (first-name-successfulp (class-add-ivar c "firstName" (foreign-type-size :pointer)
						 (coerce (floor (log (foreign-type-size :pointer) 2))
							 'integer) (null-pointer)))
	 (last-name-successfulp (class-add-ivar c "lastName" (foreign-type-size :pointer)
						(coerce (floor (log (foreign-type-size :pointer) 2))
							'integer) (encode-type :id)))
	 (age-successfulp (class-add-ivar c "age" (foreign-type-size :unsigned-int)
					  (coerce (floor (log (foreign-type-size :unsigned-int) 2))
						  'integer) (encode-type :id)))
	 (first-name-ivar (class-get-instance-variable c "firstName"))
	 (last-name-ivar (class-get-instance-variable c "lastName"))
	 (age-ivar (class-get-instance-variable c "age"))
	 (age-offset (ivar-get-offset age-ivar)))
    
    (defimp test-class-init (first-name last-name age)
      (object-set-ivar self first-name-ivar first-name)
      (object-set-ivar self last-name-ivar last-name)
      (setf (mem-ref age-offset :unsigned-int) age)
      self)

    (let ((type-string (apply #'concatenate 'string
			      (mapcar #'encode-type
				      '(:id :id :sel :id :id :uint)))))
      (class-add-method c (sel-register-name "initWithFirstName:lastName:age:")
			(callback test-class-init)
			type-string))
    
|#
