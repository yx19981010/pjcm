package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 发票信息导出列表项
 *
 * @author hujiahao
 */
@Data
public class PostExport4InvoiceVO {
    @ApiModelProperty("发票抬头")
    private String invoiceTitle;

    @ApiModelProperty("纳税人识别号")
    private String taxpayerId;

    @ApiModelProperty("金额")
    private Double fee;

    @ApiModelProperty("税率")
    private Double taxRate;

    @ApiModelProperty("通讯作者")
    private String corAuName;

    @ApiModelProperty("稿号")
    private String no;
}
