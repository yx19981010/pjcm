package com.samsph.pjcm.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.util.Date;

/**
 * @author hujiahao
 */
@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "review_record")
public class ReviewRecord {
    /**
     * 标识
     */
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    /**
     * 稿件id
     */
    @Column(name = "pid")
    private Integer pid;

    /**
     * 审稿人id
     */
    @Column(name = "uid")
    private Integer uid;

    /**
     * 属于第几轮审阅
     */
    @Column(name = "count")
    private Integer count;

    /**
     * 记录创建时间
     */
    @Column(name = "create_time")
    private Date createTime;

    /**
     * 是否否决
     */
    @Column(name = "reject")
    private Integer reject;

    /**
     * 否决意见
     */
    @Column(name = "reject_comment")
    private String rejectComment;

    /**
     * 是否要转送
     */
    @Column(name = "to_forward")
    private Integer toForward;

    /**
     * 转送意见
     */
    @Column(name = "forward_comment")
    private String forwardComment;

    /**
     * 是否要修改
     */
    @Column(name = "to_revise")
    private Integer toRevise;

    /**
     * 修改意见
     */
    @Column(name = "revise_comment")
    private String reviseComment;

    /**
     * 给编辑部的建议
     */
    @Column(name = "advice_to_newsroom")
    private String adviceToNewsroom;

    /**
     * 政治方面
     */
    @Column(name = "political")
    private Integer political;

    /**
     * 学术方面
     */
    @Column(name = "academic")
    private Integer academic;

    /**
     * 科学性方面
     */
    @Column(name = "scientific")
    private Integer scientific;

    /**
     * 文字方面
     */
    @Column(name = "text")
    private Integer text;

    /**
     * 实用性方面
     */
    @Column(name = "practicality")
    private Integer practicality;

    /**
     * 总评价
     */
    @Column(name = "evaluation")
    private Integer evaluation;
}
