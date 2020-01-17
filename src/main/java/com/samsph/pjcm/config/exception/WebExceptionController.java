package com.samsph.pjcm.config.exception;

import com.samsph.pjcm.config.utils.AddressIpUtil;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.validation.BindException;
import org.springframework.validation.FieldError;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.ConstraintViolationException;
import java.io.BufferedReader;
import java.io.PrintWriter;
import java.io.StringWriter;

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

    @Resource
    private HttpServletRequest request;

    // 当数据校验失败时，会抛出BindException或MethodArgumentNotValidException，
    // 为防止防止重复编码，对这两种异常做全局处理。

    @ResponseBody
    @ExceptionHandler(BindException.class)
    public AjaxResponse handleBindException(BindException ex) {
        FieldError fieldError = ex.getBindingResult().getFieldError();
        return AjaxResponse.error(new CustomException(CustomExceptionType.USER_INPUT_ERROR, fieldError.getDefaultMessage()));
    }

    @ResponseBody
    @ExceptionHandler(MethodArgumentNotValidException.class)
    public AjaxResponse handleMethodArgumentNotValidException(MethodArgumentNotValidException ex) {
        FieldError fieldError = ex.getBindingResult().getFieldError();
        return AjaxResponse.error(new CustomException(CustomExceptionType.USER_INPUT_ERROR, fieldError.getDefaultMessage()));
    }

    // 不支持的HTTP方法异常

    @ResponseBody
    @ExceptionHandler(HttpRequestMethodNotSupportedException.class)
    public AjaxResponse handleHttpRequestMethodNotSupportedException(HttpRequestMethodNotSupportedException ex) {
        return AjaxResponse.error(new CustomException(CustomExceptionType.USER_INPUT_ERROR, ex.getMessage()));
    }

    // 请求参数(?page=1)缺失异常

    @ResponseBody
    @ExceptionHandler(MissingServletRequestParameterException.class)
    public AjaxResponse handleMissingServletRequestParameterException(MissingServletRequestParameterException ex) {
        return AjaxResponse.error(new CustomException(CustomExceptionType.USER_INPUT_ERROR, ex.getMessage()));
    }


    // 请求参数检验不通过（如@NotNull却page=）

    @ResponseBody
    @ExceptionHandler(ConstraintViolationException.class)
    public AjaxResponse handleBindException(ConstraintViolationException ex) {
        return AjaxResponse.error(new CustomException(CustomExceptionType.USER_INPUT_ERROR, ex.getMessage()));
    }

    @ResponseBody
    @ExceptionHandler(CustomException.class)
    public AjaxResponse customerException(CustomException e) {
        if (e.getCode() == CustomExceptionType.SYSTEM_ERROR.getCode()) {

            // 将500异常信息持久化处理，方便运维人员处理
            log.error("\n500 Internal Server Error!\n" +
                    request2Str(request) + "\n" +
                    exception2Str(e) + "\n"+
                    "---------------------------------------------------------------------------------------------------------\n");
        }
        return AjaxResponse.error(e);
    }

    @ResponseBody
    @ExceptionHandler(AccessDeniedException.class)
    public AjaxResponse exception(AccessDeniedException ex) {
        return AjaxResponse.error(new CustomException(CustomExceptionType.USER_INPUT_ERROR, ex.getMessage()));
    }

    @ResponseBody
    @ExceptionHandler(Exception.class)
    public AjaxResponse exception(Exception e) {

        // 将999异常信息持久化处理，方便运维人员处理
        log.error("\n999 Unknown Error!\n" +
                request2Str(request) + "\n" +
                exception2Str(e) + "\n"+
                "---------------------------------------------------------------------------------------------------------\n");

        return AjaxResponse.error(new CustomException(CustomExceptionType.OTHER_ERROR, "未知异常"));
    }

    private String exception2Str(Exception e) {
        if (e == null) {
            return "";
        }
        StringWriter stringWriter = new StringWriter();
        e.printStackTrace(new PrintWriter(stringWriter));
        return "异常信息：" + stringWriter.toString();
    }

    @SneakyThrows
    private String request2Str(HttpServletRequest q) {
        if (q == null) {
            return "";
        }
        String str;
        StringBuilder wholeStr = new StringBuilder();
        wholeStr.append("IP地址：").append(AddressIpUtil.getIpAddress(q)).append("\n")
                .append("请求方法：").append(request.getMethod()).append("\n")
                .append("请求URL:").append(q.getRequestURL());
//                .append("请求实体：");

//        BufferedReader br = q.getReader();
//        while ((str = br.readLine()) != null) {
//            wholeStr.append(str);
//        }

        return wholeStr.toString();
    }
}
