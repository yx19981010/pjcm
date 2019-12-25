package com.samsph.pjcm.config.exception;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * 统一响应前端的数据结构
 *
 * @author hujiahao
 */
@ApiModel(description="Ajax响应实体类")
@Data
public class AjaxResponse {
    @ApiModelProperty("Ajax请求是否成功")
    private boolean isok;

    @ApiModelProperty("HTTP状态码，有200（成功）、400（客户端错误）、500（服务器错误）、999（未知错误）四种")
    private int code;

    @ApiModelProperty("请求失败的的提示信息")
    private String message;

    @ApiModelProperty("请求成功时，需要响应给前端的数据")
    private Object data;

    @ApiModelProperty("响应时间")
    private Date timeStamp;

    private AjaxResponse() {
    }

    /**
     * 请求出现异常时的响应数据封装
     *
     * @param e 自定义异常对象
     * @return AjaxResponse
     */
    static AjaxResponse error(CustomException e) {
        AjaxResponse resultBean = new AjaxResponse();
        resultBean.setIsok(false);
        resultBean.setCode(e.getCode());
        resultBean.setTimeStamp(new Date());
        if(e.getCode() == CustomExceptionType.USER_INPUT_ERROR.getCode()){
            resultBean.setMessage(e.getMessage());
        }else if(e.getCode() == CustomExceptionType.SYSTEM_ERROR.getCode()){
            resultBean.setMessage(e.getMessage() + ",系统出现异常，请联系管理员电话：***********进行处理!");
        }else{
            resultBean.setMessage("系统出现未知异常，请联系管理员电话：***********进行处理!");
        }
        return resultBean;
    }

    /**
     * 请求成功时的响应数据封装，没有响应数据（比如删除、修改成功）
     *
     * @return AjaxResponse
     */
    public static AjaxResponse success() {
        AjaxResponse resultBean = new AjaxResponse();
        resultBean.setIsok(true);
        resultBean.setCode(200);
        resultBean.setMessage("OK");
        resultBean.setTimeStamp(new Date());
        return resultBean;
    }

    /**
     * 请求成功时的响应数据封装，有响应数据（比如查询成功）
     *
     * @param data 响应数据
     * @return AjaxResponse
     */
    public static AjaxResponse success(Object data) {
        AjaxResponse resultBean = new AjaxResponse();
        resultBean.setIsok(true);
        resultBean.setCode(200);
        resultBean.setMessage("OK");
        resultBean.setData(data);
        resultBean.setTimeStamp(new Date());
        return resultBean;
    }
}
