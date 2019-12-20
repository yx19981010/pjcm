package com.samsph.pjcm.query;

import com.samsph.pjcm.config.constant.MyBoolean;
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

@Data
@ApiModel(description = "上传收据信息请求实体\n必填：id, invoiceNeeded。若需要发票，则其他字段也需填写完整。")
public class PostReceiptQuery {
    @ApiModelProperty("标识号")
    @NotNull(message = "id不能为空")
    private Integer id;

    @ApiModelProperty("是否需要发票")
    @NotNull(message = "receiptedNeeded不能为空")
    private boolean invoiceNeeded;

    @ApiModelProperty("发票抬头")
    private String invoiceTitle;

    @ApiModelProperty("纳税人识别号")
    private String taxpayerId;

    @ApiModelProperty("发票收件地址")
    private String receiptAddress;

    @ApiModelProperty("发票收件人")
    private String receiptReceiver;

    @ApiModelProperty("收件人手机号")
    private String receiverPhone;
}