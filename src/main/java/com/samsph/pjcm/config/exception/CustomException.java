package com.samsph.pjcm.config.exception;

import lombok.Getter;

/**
 * 自定义异常类
 *
 * @author hujiahao
 */
public class CustomException extends RuntimeException {
    /**
     * 异常错误编码
     */
    @Getter
    private int code ;
    /**
     * 异常信息
     */
    @Getter
    private String message;

    public CustomException(CustomExceptionType exceptionTypeEnum, String message) {
        this.code = exceptionTypeEnum.getCode();
        this.message = message;
    }
}
