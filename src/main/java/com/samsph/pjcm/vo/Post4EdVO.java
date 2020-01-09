package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import java.util.Date;

/**
 * @author hujiahao
 */
@Data
@EqualsAndHashCode(callSuper = true)
@ApiModel(description = "稿件VO-编辑")
public class Post4EdVO extends Post4EdSimpleVO{
    /**
     * 稿件审阅轮数
     */
    private Integer count;

    /**
     * 投稿领域
     */
    private Integer field;

    /**
     * 第一作者单位
     */
    private String fAuEmployer;

    /**
     * 第一作者邮箱
     */
    private String fAuEmail;

    /**
     * 第一作者手机号
     */
    private String fAuPhone;

    /**
     * 第一作者职称
     */
    private String fAuTitle;

    /**
     * 第一作者学历
     */
    private String fAuEducation;

    /**
     * 通讯作者姓名
     */
    private String corAuName;

    /**
     * 作者信息
     */
    private String writersInfo;

    /**
     * 体裁
     */
    private Integer genre;

    /**
     * 基金级别
     */
    private Integer fundLevel;

    /**
     * 稿件英文标题
     */
    private String titleEn;

    /**
     * 中文关键词
     */
    private String keywordsZh;

    /**
     * 英文关键词
     */
    private String keywordsEn;

    /**
     * 中文摘要
     */
    private String abstractZh;

    /**
     * 英文摘要
     */
    private String abstractEn;

    /**
     * 参考文献
     */
    private String referencesList;

    /**
     * 创建时间
     */
    private Date createTime;

    /**
     * 稿件上传时间
     */
    private Date postUploadTime;

    /**
     * 推荐信上传时间
     */
    private Date letterUploadTime;

    /**
     * 伦理委员会批文上传时间
     */
    private Date ethicsApprovalUploadTime;

    /**
     * 基金课题批文上传时间
     */
    private Date fundApprovalUploadTime;

    /**
     * 编辑意见
     */
    private String editorComment;

    /**
     * 编辑提出意见时间
     */
    private Date editorCommentTime;

    /**
     * 稿件编号
     */
    private String no;

    /**
     * 税率
     */
    private Double taxRate;

    /**
     * 版面费
     */
    private Double fee;

    /**
     * 发录用通知时间
     */
    private Date acceptanceNoticeTime;

    /**
     * 缴费证明上传时间
     */
    private Date certificateUploadTime;

    /**
     * 著作权转让书上传时间
     */
    private Date assignmentUploadTime;

    /**
     * 是否需要发票
     */
    private Integer invoiceNeeded;

    /**
     * 发票抬头
     */
    private String invoiceTitle;

    /**
     * 纳税人识别号
     */
    private String taxpayerId;

    /**
     * 发票邮寄地址
     */
    private String receiptAddress;

    /**
     * 发票邮寄收件人
     */
    private String receiptReceiver;

    /**
     * 收件人手机号
     */
    private String receiverPhone;

    /**
     * 缴费证明的审阅意见
     */
    private String certificateComment;

    /**
     * 缴费证明的审阅时间
     */
    private Date certificateCommentTime;

    /**
     * 缴费截止时间
     */
    private Date deadline;
}
