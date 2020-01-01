//
//  ExceptionWrapper.h
//  ExceptionWrapper
//
//  Created by Jeremy Phelps on 12/31/19.
//  Copyright © 2019 Jeremy Phelps. All rights reserved.
//

#import <Foundation/Foundation.h>

// The exception_wrapper function allows one to call
// Objective-C functions from Lisp that might throw
// Objective-C exceptions, which there is no way to
// catch from Lisp. Using it solves one of the problems
// of interfacing Objective-C to Lisp, but there are still
// many other problems.
//
// Parameters:
// - thunk: A function with the following prototype:
//          void thunk(void* datum);
//
//          The {datum} argument you pass to exception_wrapper()
//          will be passed through to the {thunk} function. This
//          is the function for which Objective-C exceptions will be
//          caught.
//
// - cleanup: Either a null pointer, or a function with the following prototype:
//            void cleanup(int success, void *datum);
//
//            If {cleanup} is a null pointer, it's ignored.
//
//            The {cleanup} function is called from an Objective-C
//            @finally block.
//
//            The {success} integer is 0 if an exception was thrown,
//            or 1 if no exception was thrown.
//
//            The same {datum} that is passed to the {thunk} function
//            will also be passed to the {cleanup} function.
//
// Usage from Lisp:
//
// Using CFFI, one can do this:
//
// (cffi:defcallback my-callback :void ((datum :pointer))
//    ;; Code that may throw Objective-C
//    ;; exceptions
//  )
//
// (cffi:defcallback my-cleanup :void ((success :int)
//                                     (datum :pointer))
//   ;; Cleanup
// )
//
// (defparameter *exception* (exception_wrapper (cffi:callback my-callback) (cffi:callback my-cleanup))
//
// The result will be a CFFI null-pointer if no exception was thrown, otherwise
// a pointer to the NSException object.
//
// It is not safe to use UNWIND-PROTECT, special variables, INVOKE-RESTART, or THROW from within
// the dynamic extent of MY-CALLBACK, or any other Lisp call that can trigger
// any sort of exit from MY-CALLBACK other than by a normal return from the
// function.
//
//  - If an Objective-C exception is thrown from the VALUE-FORM of an UNWIND-PROTECT,
//    the BODY of the UNWIND-PROTECT may not be executed. This is true because the
//    Objective-C compiler may implement its exceptions in a way that is not compatible
//    with whatever your Lisp compiler does to ensure that UNWIND-PROTECT works.
//
//    The same problem may exist with special variables: If you bind one with LET and
//    then an Objective-C exception is thrown that unwinds your LET form, the code to
//    restore the special variable's value might get skipped.
//
//    The {cleanup} callback is a workaround for some of the problems. 
//
//  - If your code unwinds multiple stack frames via THROW, INVOKE-RESTART, RETURN-FROM, etc,
//    there's no telling if the stack hackery implemented by Apple's Objective-C compiler
//    to support things like exceptions and ARC will hold up. Stack corruption may result
//    from doing this.
//
//  - SBCL generates simple assembly code for simplee Lisp code. It should be safe for
//    an Objective-C exception to unwind your callback's stack frame
id exception_wrapper(void (*thunk)(void*), void (*cleanup)(int success, void*), void *datum);
