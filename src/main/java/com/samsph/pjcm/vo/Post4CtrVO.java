package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.persistence.Column;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import java.util.Date;

/**
 * @author hujahao
 */

@Data
@EqualsAndHashCode(callSuper = true)
@ApiModel(description = "稿件VO-投稿人")
public class Post4CtrVO extends Post4CtrSimpleVO {

    @ApiModelProperty("稿件英文标题")
    private String titleEn;

    @ApiModelProperty("作者信息")
    private String writersInfo;

    @ApiModelProperty("投稿领域")
    private Integer field;

    @ApiModelProperty("体裁")
    private Integer genre;

    @ApiModelProperty("基金级别")
    private Integer fundLevel;

    @ApiModelProperty("中文关键词")
    private String keywordsZh;

    @ApiModelProperty("英文关键词")
    private String keywordsEn;

    @ApiModelProperty("中文摘要")
    private String abstractZh;

    @ApiModelProperty("英文摘要")
    private String abstractEn;

    @ApiModelProperty("稿件状态")
    private Integer status;

    @ApiModelProperty("稿件审阅总轮数")
    private Integer count;

    @ApiModelProperty("创建时间")
    private Date createTime;

    @ApiModelProperty("稿件上传时间")
    private Date postUploadTime;

    @ApiModelProperty("推荐信上传时间")
    private Date letterUploadTime;

    @ApiModelProperty("伦理委员会批文上传时间")
    private Date ethicsApprovalUploadTime;

    @Column(name = "基金课题批文上传时间")
    private Date fundApprovalUploadTime;

    @ApiModelProperty("提交初审时间")
    private Date submitTime;

    @ApiModelProperty("初审意见")
    private String firstExamComment;

    @ApiModelProperty("初审时间")
    private Date firstExamCommentTime;

    @ApiModelProperty("退稿意见")
    private String rejectComment;

    @ApiModelProperty("退稿时间")
    private Date rejectCommentTime;

    @ApiModelProperty("发表前审阅意见")
    private String bfPubComment;

    @ApiModelProperty("稿发表前审阅时间")
    private Date bfPubCommentTime;

    @ApiModelProperty("格式审阅意见")
    private String formatComment;

    @ApiModelProperty("格式审阅时间")
    private Date formatCommentTime;

    @ApiModelProperty("版面费")
    private Double fee;

    @ApiModelProperty("缴费证明上传时间")
    private Date certificateUploadTime;

    @ApiModelProperty("是否需要发票")
    private Integer invoiceNeeded;

    @ApiModelProperty("发票抬头")
    private String invoiceTitle;

    @ApiModelProperty("纳税人识别号")
    private String taxpayerId;

    @ApiModelProperty("发票邮寄地址")
    private String receiptAddress;

    @ApiModelProperty("发票邮寄收件人")
    private String receiptReceiver;

    @ApiModelProperty("发票收件人手机")
    private String receiverPhone;

    @ApiModelProperty("缴费证明的审阅意见")
    private String certificateComment;

    @ApiModelProperty("缴费证明的审阅时间")
    private Date certificateCommentTime;
}
