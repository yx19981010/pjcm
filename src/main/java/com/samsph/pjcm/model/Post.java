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
     * 投稿领域
     */
    @Column(name = "field")
    private Integer field;

    /**
     * 第一作者姓名
     */
    @Column(name="f_au_name")
    private String fAuName;

    /**
     * 第一作者单位
     */
    @Column(name="f_au_employer")
    private String fAuEmployer;

    /**
     * 第一作者邮箱
     */
    @Column(name="f_au_email")
    private String fAuEmail;

    /**
     * 第一作者手机号
     */
    @Column(name="f_au_phone")
    private String fAuPhone;

    /**
     * 第一作者职称
     */
    @Column(name="f_au_title")
    private String fAuTitle;

    /**
     * 第一作者学历
     */
    @Column(name="f_au_education")
    private String fAuEducation;

    /**
     * 通讯作者姓名
     */
    @Column(name="cor_au_name")
    private String corAuName;

    /**
     * 作者信息
     */
    @Column(name = "writers_info")
    private String writersInfo;

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
     * 稿件标题
     */
    @Column(name = "title")
    private String title;

    /**
     * 稿件英文标题
     */
    @Column(name = "title_en")
    private String titleEn;

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
     * 参考文献
     */
    @Column(name="references_list")
    private String referencesList;

    /**
     * 投稿人id
     */
    @Column(name = "contributor_uid")
    private Integer contributorUid;

    /**
     * 创建时间
     */
    @Column(name = "create_time")
    private Date createTime;

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
    @Column(name = "submit_time")
    private Date submitTime;

    /**
     * 编辑id
     */
    @Column(name = "editor_uid")
    private Integer editorUid;

    /**
     * 编辑意见
     */
    @Column(name = "editor_comment")
    private String editorComment;

    /**
     * 编辑提出意见时间
     */
    @Column(name = "editor_comment_time")
    private Date editorCommentTime;

    /**
     * 稿件编号
     */
    @Column(name = "no")
    private String no;

    /**
     * 税率
     */
    @Column(name="tax_rate")
    private Double taxRate;

    /**
     * 版面费
     */
    @Column(name = "fee")
    private Double fee;

    /**
     * 发录用通知时间
     */
    @Column(name="acceptance_notice_time")
    private Date acceptanceNoticeTime;

    /**
     * 录用通知文件路径
     */
    @Column(name="acceptance_notice_path")
    private String acceptanceNoticePath;

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
     * 著作权转让书的文件路径
     */
    @Column(name = "assignment_path")
    private String assignmentPath;

    /**
     * 著作权转让书上传时间
     */
    @Column(name = "assignment_upload_time")
    private Date assignmentUploadTime;

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
     * 收件人手机号
     */
    @Column(name = "receiver_phone")
    private String receiverPhone;

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
}
