package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * 稿件导出列表项
 *
 * @author hujiahao
 */
@Data
public class PostExportVO {
    @ApiModelProperty("论文编号")
    private String no;

    @ApiModelProperty("标题")
    private String title;

    @ApiModelProperty("投稿时间")
    private Date submitTime;

    @ApiModelProperty("状态")
    private Integer status;

    @ApiModelProperty("发录用通知时间")
    private Date acceptanceNoticeTime;

    @ApiModelProperty("版面费")
    private Double fee;

    @ApiModelProperty("发票抬头")
    private String invoiceTitle;

    @ApiModelProperty("纳税人识别号")
    private String taxpayerId;

    @ApiModelProperty("一作名")
    private String fAuName;

    @ApiModelProperty("单位")
    private String fAuEmployer;

    @ApiModelProperty("手机号")
    private String fAuPhone;

    @ApiModelProperty("邮箱")
    private String fAuEmail;

    @ApiModelProperty("职称")
    private String fAuTitle;

    @ApiModelProperty("学历")
    private String fAuEducation;

    @ApiModelProperty("地区")
    private String receiptAddress;
}
