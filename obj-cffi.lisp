(cl:defpackage #:obj-cffi
  (:use #:closer-common-lisp #:cffi #:uiop/utility))

(cl:in-package #:obj-cffi)

;; An OSX app in plain C: https://github.com/jimon/osx_app_in_plain_c
;;
;; It uses the COCOA API.

;;
;; The NEXT browser's upcoming GUI toolkit may obviate the need for calling
;; Objective-C from Lisp.

(defparameter *test-lib* (load-foreign-library #P"/Users/jeremyphelps/Library/Developer/Xcode/DerivedData/Testing-gteieshxlukrkbdowshxnxgffxkl/Build/Products/Debug/libTesting.dylib"))

;; You may find that libExceptionWrapper.dylib must be installed in /usr/local/lib for this
;; to work. When I load this project on macOS with ASDF, I find that the FASL gets compiled
;; to ~/.cache/common-lisp/sbcl-1.5.1-macosx-x84/..../obj-cffi.fasl, which makes it impossible
;; to load another file from the same directory as the Lisp source file.

(defvar *exception-wrapper* (restart-case
				(handler-bind ((load-foreign-library-error
						(lambda (exn)
						  (declare (ignore exn))
						  (ignore-errors
						    (progn
						      (format t "~%libExceptionWrapper.dylib not found in standard directories; trying to load from ~s~%" (merge-pathnames #P"libExceptionWrapper.dylib" *load-truename*))
						      (invoke-restart 'continue (load-foreign-library (merge-pathnames #P"libExceptionWrapper.dylib" *load-truename*))))))))
				  (load-foreign-library "libExceptionWrapper.dylib"))
			      (continue (library) library)))

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

(defcfun ("objc_disposeClassPair" objc-dispose-class-pair) :void
  (class :pointer))

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

(defcfun ("object_getIvar" object-get-instance-variable-value) :pointer
  (obj :pointer)
  (ivar :pointer))

(defcfun ("object_setIvar" object-set-instance-variable-value) :void
  (obj :pointer)
  (ivar :pointer)
  (value :pointer))

(defcfun ("object_setInstanceVariable" object-set-instance-variable) :pointer
  (obj :pointer)
  (name :string)
  (value :pointer))

(defcfun ("object_getClass" object-get-class) :pointer
  (object :pointer))

(defmacro defimp (name arguments &body body)
  "Define an Objective-C IMP function. The ARGUMENTS are just a list of variable names. When
your IMP function is called, these arguments will all be CFFI :POINTERs.

The function is expected to return an Objective-C 'id' object. The generated CFFI callback
will be declared to return the type :POINTER.

The function will have an implicit first argument declared as (SELF :POINTER)."
  `(defcallback ,name :pointer ((self :pointer) (selector-name :string)
				,@arguments)
     (declare (ignorable self selector-name))
     ,@body))

(defcfun "objc_lookUpClass" (:pointer)
  (name :string))

(defcfun ("objc_registerClassPair" objc-register-class-pair) :void
  (class :pointer))

(defcfun ("class_getSuperclass" class-get-superclass) :pointer
  (class :pointer))

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

(defcfun ("ivar_getName" ivar-get-name) :string
  (ivar :pointer))

(defcfun "method_getName" :pointer
  (method :pointer))

(defcfun "sel_getName" :pointer
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

;;    SEL _Nullable name;               /**< The name of the method */
;;    char * _Nullable types;           /**< The types of the method arguments */




;; (defcstruct objc-super ...)
;; TODO: https://developer.apple.com/documentation/objectivec/objc_super?language=objc

;; Sending messages

(defmacro define-msg-send-functions ()
  `(progn
     ,@(loop for return-type in '(:int :uint :long :ulong #-(or x86-64 x86) :float #-(or x86-64 x86) :double
				  :long-long :unsigned-long-long :string
				  :pointer :void)
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

(defun objc-method-name (method)
  ;; FIXME: The string we get from this may need to be freed.
  (let ((name-pointer (sel-getname (method-getname method))))
    (if (null-pointer-p method)
	nil
	(c-string->lisp-string name-pointer))))

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
  
(defun get-direct-methods (class)
  "Given a an Objective-C CLASS, returns a list of pointers to Objective-C Method objects."
  (get-meta-list class #'class-copymethodlist))

(defun has-superclass-p (class)
  (not
   (or (eq class *ns-object*)
       (eq (class-get-superclass class) class)
       (null-pointer-p (class-get-superclass class)))))

(defun get-all-methods (class)
  (append (get-direct-methods class)
	  (if (not (has-superclass-p class))
	      nil
	      (get-all-methods (class-get-superclass class)))))

(defun get-ivars (class)
  "Given an Objective-C CLASS, returns a list of pointers to Objective-C Ivar objects."
  (get-meta-list class #'class-copyivarlist))


(defcfun ("class_getInstanceVariable" get-ivar-by-name) :pointer
  (class :pointer)
  (name :string))


(defun objc-slot-value* (object ivar &key ivar-offset (offset-type :int))
  (if ivar-offset
      (mem-ref object offset-type ivar-offset)
      (object-get-instance-variable-value object
					  ivar)))

(defun (setf objc-slot-value*) (new-value object ivar &key ivar-offset (offset-type :int))
  (if ivar-offset
      (setf (mem-ref object offset-type ivar-offset)
	    new-value)
      (progn
	(object-set-instance-variable-value object ivar
					    new-value)
	new-value)))

(defun objc-slot-value (object ivar-name &key by-offset (offset-type :int))
  (let ((ivar (class-get-instance-variable (object-get-class object) ivar-name)))
    (objc-slot-value* object ivar
		      :ivar-offset (if by-offset
				       (ivar-get-offset ivar))
		      :offset-type offset-type)))

(defun (setf objc-slot-value) (new-value object ivar-name &key by-offset (offset-type :int))
  (let ((ivar (class-get-instance-variable (object-get-class object) ivar-name)))
    (setf (objc-slot-value* object ivar
			    :offset (if by-offset
					(ivar-get-offset ivar))
			    :offset-type offset-type)
	  new-value)))

(defun get-method-by-name (class name)
  (or 
   (loop for method in (get-direct-methods class)
      when (string= name (objc-method-name method))
      return method)
   (and (has-superclass-p class)
	(get-method-by-name (class-get-superclass class) name))))

(defun methods-matching-regex (class regex)
  (append
   (loop for method in (get-direct-methods class)
      when (cl-ppcre:all-matches regex (objc-method-name method))
      collect method)
   (and (has-superclass-p class)
	(methods-matching-regex (class-get-superclass class) regex))))

(defun add-ivar (class name cffi-type objc-type)
  (unless (= (class-add-ivar class name (foreign-type-size cffi-type)
			     (coerce (floor (log (foreign-type-size cffi-type) 2))
				     'integer)
			     (encode-type objc-type))
	     1)
    (error "Unable to add instance variable ~s to class ~s" name class)))
  

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
  "Returns a Lisp string encoding the given TYPE.
  FIXME: Not all types are supported. See https://nshipster.com/type-encodings/"
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
    (:pointer "^v")
    (:class "#")))
    
(defclass objective-c-class (standard-class)
  ((class-pointer :initarg :class-pointer :initform nil)
   (c-name :type string :initarg :c-name)))

(defclass objective-c-direct-slot-definition (standard-direct-slot-definition)
  ((cffi-type :initform :pointer :initarg :cffi-type)
   (objc-type :initform :id :initarg :objc-type)
   (c-name :type string :initarg :c-name)
   (offset :type (or null integer) :initarg :offset :initform nil)
   (owning-class :initform nil)
   (ivar :initarg :ivar :initform nil)))

;; FIXME: Apparently, something extra must be done to make attributes on OBJECTIVE-C-DIRECT-SLOT-DEFINITIONs
;;        carry over to their corresponding OBJECTIVE-C-EFFECTIVE-SLOT-DEFINITIONs.
;;
;; Here is a fully working version of the SLOT-ATTRIBUTES hack shown in the AMOP book:
;;
;; https://github.com/hanshuebner/bknr-datastore/blob/master/experimental/slot-attributes.lisp
;;
;; From it, it looks like it is necessary to implement a COMPUTE-EFFECTIVE-SLOT-DEFINITION
;; that iterates over the DIRECT-SLOTS to find the slot with the expected attributes.

(defclass objective-c-effective-slot-definition (standard-effective-slot-definition)
  ((cffi-type :initform nil :initarg :cffi-type)
   (objc-type :initform :id :initarg :objc-type)
   (c-name :type string :initarg :c-name)
   (offset :type (or null integer) :initarg :offset :initform nil)
   (owning-class :initform nil)
   (ivar :initarg :ivar :initform nil)))


(defmethod validate-superclass ((class objective-c-class)
				(super standard-class))
  t)

(defmethod validate-superclass ((class objective-c-direct-slot-definition)
				(super standard-direct-slot-definition))
  t)

(defmethod validate-superclass ((class objective-c-effective-slot-definition)
				(super standard-effective-slot-definition))
  t)


(defmethod direct-slot-definition-class ((class objective-c-class) &rest initargs)
  (declare (ignore initargs))
  'objective-c-direct-slot-definition)

(defmethod effective-slot-definition-class ((class objective-c-class) &rest initargs)
    'objective-c-effective-slot-definition)

(defmacro check-slot-presence (class name obj slot)
  `(unless (slot-value ,obj ,slot)
     (error "Slot ~s in class ~s needs a ~s~%"
	    ,name ,class
	    (intern (symbol-name ,slot) :keyword))))

(defun validate-slot-definition (class name slot-def)
  (loop for slot in '(cffi-type objc-type)
     do (check-slot-presence class name slot-def slot)))

(defun objective-c-superclasses (class)
  (handler-case
      (remove-if-not
       (lambda (class*)
	 (and (not (eq class* class))
	      (typep class* 'objective-c-class)))
       (class-precedence-list class))
    (unbound-slot () nil)))

(defparameter *direct-slots* nil)

(defun infer-cffi-type (objc-type)
  (case objc-type
    ((:id :sel :selector :class)
     :pointer)
    (:bool :int)
    (otherwise objc-type)))

(defmethod compute-effective-slot-definition :around ((class objective-c-class) name direct-slots)
  (let ((effective-slot (call-next-method)))
    (if (typep effective-slot 'objective-c-effective-slot-definition)
	(progn
	  (unless (slot-value effective-slot 'cffi-type)
	    (setf (slot-value effective-slot 'cffi-type)
		  (infer-cffi-type (slot-value effective-slot 'objc-type))))
	  (validate-slot-definition class name effective-slot)
	  (unless (= (length direct-slots) 1)
	    (error "Duplicate Objective-C slot ~s in Objective-C class ~s" name class))
	  (loop for slot in '(cffi-type objc-type c-name offset)
	     do (setf (slot-value effective-slot slot)
		      (slot-value (car direct-slots) slot)))
	  effective-slot)
	effective-slot)))

(defun create-ivar-for-slot (class effective-slot-definition)
  (with-slots (cffi-type objc-type c-name offset ivar)
      effective-slot-definition
    (add-ivar (slot-value class 'class-pointer)
	      c-name cffi-type objc-type)
    (setf ivar (get-ivar-by-name (slot-value class 'class-pointer) c-name))
    (setf offset (ivar-get-offset ivar))))

(defvar *objc-class-to-lisp-class* (make-hash-table :test 'eq))

(defun create-objective-c-parts (objective-c-class class-precedence-list class-slots)
  (unless (slot-value objective-c-class 'class-pointer)
    (let ((supers (remove-if-not
		   (lambda (class)
		     (typep class 'objective-c-class))
		   (cdr class-precedence-list))))
      (let ((objc-class (objc-alloc-class-pair (if supers
						   (slot-value (car supers) 'class-pointer)
						   *ns-object*)
					       (car (slot-value objective-c-class 'c-name))
					       0)))
	(if (null-pointer-p objc-class)
	    (setf objc-class (objc-lookupclass (car (slot-value objective-c-class 'c-name)))))
	(if (null-pointer-p objc-class)
	    (error "Unable to create Objective-C class for CLOS class ~s"
		   objective-c-class))
	
	(setf (slot-value objective-c-class 'class-pointer)
	      objc-class))
      
      (loop for slot-def in class-slots
	 unless (or
		 (not (typep slot-def 'objective-c-effective-slot-definition))
		 (slot-value slot-def 'ivar))
	 do (create-ivar-for-slot objective-c-class slot-def))))
  (setf (gethash (slot-value objective-c-class 'class-pointer)
		 *objc-class-to-lisp-class*)
	objective-c-class))

(defmethod finalize-inheritance :before ((class objective-c-class))
  (format t "~%Finalizing inheritance for ~s~%" class)
  (mapc #'finalize-inheritance (objective-c-superclasses class)))

(defun make-foreign-pointer-slot (class)
  (declare (ignore class))
  (let ((slotd (make-instance 'standard-effective-slot-definition
			      :name 'foreign-pointer)))
    slotd))

(defmethod compute-slots ((class objective-c-class))
  (let ((existing-slots (call-next-method)))
    (create-objective-c-parts class (class-precedence-list class)
			      existing-slots)
    (objc-register-class-pair (slot-value class 'class-pointer))
    
    (if (find 'foreign-pointer existing-slots
	      :key #'slot-definition-name)
	existing-slots
	(cons (make-foreign-pointer-slot class)
	      existing-slots))))

(defmethod allocate-instance :around ((class objective-c-class) &rest initargs)
;;  (declare (ignore initargs))
  (format t "~%Allocating an instance of ~s instantiated with ~s~%" class initargs)
  (let ((instance (call-next-method)))
    (declare (optimize (debug 3)))
    (setf (slot-value instance 'foreign-pointer)
	  (%send (slot-value class 'class-pointer)
		 (method-getname
		  (get-method-by-name
		   (object-get-class (slot-value class 'class-pointer))
		   "alloc"))
		 :pointer
		 nil))
    instance))


;; However, there's a second problem: Objective-C code can
;; assign objects to IVARs for which we have no CLOS wrapper
;; defined. I suppose it would be legitimate to wrap such
;; pointers in instances of the NS-OBJECT class. But what
;; if somebody later defines a class? It would be hard to
;; find an opportunity to detect such an event.

(defun wrap-ivar-value-in-clos-object (value containing-object clos-wrapper slot-definition)
  "In reality, Objective-C objects contain two sets of slots:

     1. Lisp slots.
     2. Objective-C Ivars.

   When you use SLOT-VALUE to assign to an Objective-C object, this library
   copies the value to both the Lisp slot and the Objective-C Ivar.

   When accessing the slot, it generally only reads the Objective-C Ivar.

   But there is a problem: You can't assign CLOS objects to Objective-C Ivars.
   Only C pointers to Objective-C objects can be assigned there.

   Consequently, when you assign a CLOS object, its Objective-C pointer is what
   gets stored in the Ivar. But if we just read the Objective-C Ivar when accessing
   the slot, that means you'd get a foreign pointer back, instead of the CLOS object
   you originally assigned.

   This function is used to solve that problem. It returns the original CLOS object
   if the Ivar still has the same foreign pointer as when you last assigned it,
   otherwise it creates a new CLOS object. It attempts to look up the Lisp definition
   of the object's class, or if there isn't one, creates an NS-OBJECT object."
  (let ((original-foreign-pointer (slot-value clos-wrapper 'foreign-pointer)))
    (cond ((eq original-foreign-pointer value)
	   clos-wrapper)
	  ((not (null-pointer-p value))
	   (let* ((current-class (object-get-class value))
		  (original-class (object-get-class original-foreign-pointer))
		  (new-clos-wrapper (make-instance
				     (if (eq current-class original-class)
					 (class-of clos-wrapper)
					 (or (gethash current-class *objc-class-to-lisp-class*)
					     'ns-object)))))
	     (setf (slot-value new-clos-wrapper 'foreign-pointer) value)
	     (setf (slot-value-using-class (find-class 'standard-class)
					   containing-object
					   slot-definition)
		   new-clos-wrapper)))
	  ;; FIXME: I'm not sure what the best thing to do is with Objective-C
	  ;;        nils (aka NULL pointers). Should they be translated to Lisp
	  ;;        NIL? Perhaps not, since we really don't know if it arose from
	  ;;        a call to plain C code or not.
	  (t value))))


(defun maybe-wrap-value (value object slotd)
  (let ((clos-value (slot-value-using-class (find-class 'standard-class)
					    object
					    slotd)))
    (if (or (and (null clos-value)
		 (eq (slot-value slotd 'objc-type) :id))
	    (typep (class-of clos-value)
		   'objective-c-class))
	(wrap-ivar-value-in-clos-object value object clos-value slotd)
	value)))

(defmethod slot-value-using-class ((class objective-c-class) object (slotd objective-c-effective-slot-definition))
  (maybe-wrap-value
   (objc-slot-value* (slot-value object 'foreign-pointer)
		     (slot-value slotd 'ivar)
		     :ivar-offset (slot-value slotd 'offset)
		     :offset-type (slot-value slotd 'cffi-type))
   object
   slotd))

(defmethod (setf slot-value-using-class) :after (new-value (class objective-c-class) object (slotd objective-c-effective-slot-definition))
  (setf (objc-slot-value* (if (typep (class-of object)
				     'objective-c-class)
			      (slot-value object 'foreign-pointer)
			      object)
			  (slot-value slotd 'ivar)
			  :ivar-offset (slot-value slotd 'offset)
			  :offset-type (slot-value slotd 'cffi-type))
	new-value))


(defclass ns-object () ()
  (:metaclass objective-c-class)
  (:c-name "NSObject"))


;; TODO: Automatically determine if an offset is needed based on the type
;; TODO: Allow manual specification of whether an offset is needed.
;; TODO: Some sort of protocol to allow CLOS objects to be replaced by
;;       Objective-C objects when assigned to slots. Probably very simple
;;       to do.
;; TODO: Convert Lisp strings to Objective-C NSString objects.
#|(defclass wtf-class ()
  ((wtf-value :initarg :wtf-value :objc-type :string :c-name "da_value" :cffi-type :string))
  (:metaclass objective-c-class)
  (:c-name "WtfClass"))

(defclass wtf-subclass (wtf-class)
  ((wtf-value-2 :initarg :wtf-value :objc-type :string :c-name "da_odda_value" :cffi-type :string))
  (:metaclass objective-c-class)
  (:c-name "WtfSubClass"))
|#
