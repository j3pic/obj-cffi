//
//  ExceptionWrapper.m
//  ExceptionWrapper
//
//  Created by Jeremy Phelps on 12/31/19.
//  Copyright © 2019 Jeremy Phelps. All rights reserved.
//

#import "ExceptionWrapper.h"

id exception_wrapper(void (*thunk)(void*), void (*cleanup)(int success, void*), void *datum)
{
    int success = 1;
    @try {
        thunk(datum);
    } @catch (NSException *exception) {
        success = 0;
        return exception;
    } @finally {
        if(cleanup) {
            cleanup(success, datum);
        }
    }
    return nil;
}
