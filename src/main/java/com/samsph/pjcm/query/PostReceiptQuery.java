package com.samsph.pjcm.query;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

/**
 * @author hujiahao
 */
@ApiModel(description = "上传收据信息请求实体")
@Data
public class PostReceiptQuery {
    @ApiModelProperty("标识号")
    @NotNull(message = "id不能为空")
    private Integer id;

    @ApiModelProperty("是否需要发票")
    @NotNull(message = "receiptedNeeded不能为空")
    private Boolean receiptedNeeded;

    @ApiModelProperty("发票抬头")
    private String receiptInvoice;

    @ApiModelProperty("纳税人识别号")
    private String taxpayerId;

    @ApiModelProperty("发票收件地址")
    private String receiptAddress;

    @ApiModelProperty("发票收件人")
    private String receiptReceiver;
}