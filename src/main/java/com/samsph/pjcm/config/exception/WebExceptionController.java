package com.samsph.pjcm.config.exception;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.web.servlet.error.AbstractErrorController;
import org.springframework.boot.web.servlet.error.ErrorAttributes;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.validation.BindException;
import org.springframework.validation.FieldError;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import javax.servlet.http.HttpServletRequest;
import javax.validation.ConstraintViolationException;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;

/**
 * 全局异常处理器
 * ControllerAdvice注解监听所有的Controller，一旦抛出CustomException，
 * 就会在@ExceptionHandler(CustomException.class)对该异常进行处理。
 *
 * @author hujiahao
 */
@Slf4j
@ControllerAdvice
public class WebExceptionController {

    // 当数据校验失败时，会抛出BindException或MethodArgumentNotValidException，
    // 为防止防止重复编码，对这两种异常做全局处理。

    @ExceptionHandler(BindException.class)
    @ResponseBody
    public AjaxResponse handleBindException(BindException ex) {
        FieldError fieldError = ex.getBindingResult().getFieldError();
        return AjaxResponse.error(new CustomException(CustomExceptionType.USER_INPUT_ERROR, fieldError.getDefaultMessage()));
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    @ResponseBody
    public AjaxResponse handleMethodArgumentNotValidException(MethodArgumentNotValidException ex) {
        FieldError fieldError = ex.getBindingResult().getFieldError();
        return AjaxResponse.error(new CustomException(CustomExceptionType.USER_INPUT_ERROR, fieldError.getDefaultMessage()));
    }

    // 不支持的HTTP方法异常

    @ExceptionHandler(HttpRequestMethodNotSupportedException.class)
    @ResponseBody
    public AjaxResponse handleHttpRequestMethodNotSupportedException(HttpRequestMethodNotSupportedException ex) {
        return AjaxResponse.error(new CustomException(CustomExceptionType.USER_INPUT_ERROR, ex.getMessage()));
    }

    //     请求参数(?page=1)缺失异常

    @ExceptionHandler(MissingServletRequestParameterException.class)
    @ResponseBody
    public AjaxResponse handleMissingServletRequestParameterException(MissingServletRequestParameterException ex) {
        return AjaxResponse.error(new CustomException(CustomExceptionType.USER_INPUT_ERROR, ex.getMessage()));
    }


    // 请求参数检验不通过（如@NotNull却page=）

    @ExceptionHandler(ConstraintViolationException.class)
    @ResponseBody
    public AjaxResponse handleBindException(ConstraintViolationException ex) {
        return AjaxResponse.error(new CustomException(CustomExceptionType.USER_INPUT_ERROR, ex.getMessage()));
    }

    @ExceptionHandler(CustomException.class)
    @ResponseBody
    public AjaxResponse customerException(CustomException ex) {
        if (ex.getCode() == CustomExceptionType.SYSTEM_ERROR.getCode()) {
            // 400异常不需要持久化，将异常信息以友好的方式告知用户即可

            // TODO: 将500异常信息持久化处理，方便运维人员处理
        }
        return AjaxResponse.error(ex);
    }

    @ExceptionHandler(AccessDeniedException.class)
    @ResponseBody
    public AjaxResponse exception(AccessDeniedException ex) {
        return AjaxResponse.error(new CustomException(CustomExceptionType.USER_INPUT_ERROR, ex.getMessage()));
    }

//    @ExceptionHandler(Exception.class)
//    @ResponseBody
//    public AjaxResponse exception(Exception e) {
//        // TODO: 将999异常信息持久化处理，方便运维人员处理
//        return AjaxResponse.error(new CustomException(CustomExceptionType.OTHER_ERROR, "未知异常"));
//    }
}
