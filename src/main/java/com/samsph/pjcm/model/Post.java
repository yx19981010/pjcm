package com.samsph.pjcm.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.util.Date;

/**
 * 稿件表实体类
 *
 * @author hujahao
 */
@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "post")
public class Post {
    /**
     * 标识
     */
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    /**
     * 稿件标题
     */
    @Column(name = "title")
    private String title;

    /**
     * 稿件英文标题
     */
    @Column(name = "titleEn")
    private String titleEn;

    /**
     * 作者信息
     */
    @Column(name = "writers_info")
    private String writersInfo;

    /**
     * 投稿人id
     */
    @Column(name = "contributor_uid")
    private Integer contributorUid;

    /**
     * 投稿领域
     */
    @Column(name = "field")
    private Integer field;

    /**
     * 体裁
     */
    @Column(name = "genre")
    private Integer genre;

    /**
     * 基金级别
     */
    @Column(name = "fund_level")
    private Integer fundLevel;

    /**
     * 中文关键词
     */
    @Column(name = "keywords_zh")
    private String keywordsZh;

    /**
     * 英文关键词
     */
    @Column(name = "keywords_en")
    private String keywordsEn;

    /**
     * 中文摘要
     */
    @Column(name = "abstract_zh")
    private String abstractZh;

    /**
     * 英文摘要
     */
    @Column(name = "abstract_en")
    private String abstractEn;

    /**
     * 稿件状态
     */
    @Column(name = "status")
    private Integer status;

    /**
     * 稿件审阅轮数
     */
    @Column(name = "count")
    private Integer count;

    /**
     * 创建时间
     */
    @Column(name = "create_time")
    private Date createTime;

    /**
     * 编辑id
     */
    @Column(name = "editor_uid")
    private Integer editorUid;

    /**
     * 稿件路径
     */
    @Column(name = "post_path")
    private String postPath;

    /**
     * 稿件上传时间
     */
    @Column(name = "post_upload_time")
    private Date postUploadTime;

    /**
     * 推荐信的文件路径
     */
    @Column(name = "letter_path")
    private String letterPath;

    /**
     * 推荐信上传时间
     */
    @Column(name = "letter_upload_time")
    private Date letterUploadTime;

    /**
     * 伦理委员会批文的文件路径
     */
    @Column(name = "ethics_approval_path")
    private String ethicsApprovalPath;

    /**
     * 伦理委员会批文上传时间
     */
    @Column(name = "ethics_approval_upload_time")
    private Date ethicsApprovalUploadTime;

    /**
     * 基金课题批文的文件路径
     */
    @Column(name = "fund_approval_path")
    private String fundApprovalPath;

    /**
     * 基金课题批文上传时间
     */
    @Column(name = "fund_approval_upload_time")
    private Date fundApprovalUploadTime;

    /**
     * 提交初审时间
     */
    @Temporal(TemporalType.DATE)
    @Column(name = "submit_time")
    private Date submitTime;

    /**
     * 初审意见
     */
    @Column(name = "first_exam_comment")
    private String firstExamComment;

    /**
     * 初审时间
     */
    @Column(name = "first_exam_comment_time")
    private Date firstExamCommentTime;

    /**
     * 退稿意见
     */
    @Column(name = "reject_comment")
    private String rejectComment;

    /**
     * 退稿时间
     */
    @Column(name = "reject_comment_time")
    private Date rejectCommentTime;

    /**
     * 发表前审阅意见
     */
    @Column(name = "bf_pub_comment")
    private String bfPubComment;

    /**
     * 发表前审阅时间
     */
    @Column(name = "bf_pub_comment_time")
    private Date bfPubCommentTime;

    /**
     * 格式审阅意见
     */
    @Column(name = "format_comment")
    private String formatComment;

    /**
     * 格式审阅时间
     */
    @Column(name = "format_comment_time")
    private Date formatCommentTime;

    /**
     * 版面费
     */
    @Column(name = "fee")
    private Double fee;

    /**
     * 缴费证明的文件路径
     */
    @Column(name = "certificate_path")
    private String certificatePath;

    /**
     * 缴费证明上传时间
     */
    @Column(name = "certificate_upload_time")
    private Date certificateUploadTime;

    /**
     * 是否需要发票
     */
    @Column(name = "invoice_needed")
    private Integer invoiceNeeded;

    /**
     * 发票抬头
     */
    @Column(name = "invoice_title")
    private String invoiceTitle;

    /**
     * 纳税人识别号
     */
    @Column(name = "taxpayer_id")
    private String taxpayerId;

    /**
     * 发票邮寄地址
     */
    @Column(name = "receipt_address")
    private String receiptAddress;

    /**
     * 发票邮寄收件人
     */
    @Column(name = "receipt_receiver")
    private String receiptReceiver;

    /**
     * 缴费证明的审阅意见
     */
    @Column(name = "certificate_comment")
    private String certificateComment;

    /**
     * 缴费证明的审阅时间
     */
    @Column(name = "certificate_comment_time")
    private Date certificateCommentTime;

    /**
     * 所属期刊id
     */
    @Column(name = "jid")
    private Integer jid;
}
