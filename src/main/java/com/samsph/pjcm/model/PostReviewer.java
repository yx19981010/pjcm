package com.samsph.pjcm.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.util.Date;

/**
 * 审稿人-稿件表实体类
 *
 * @author hujiahao
 */
@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "post_reviewer")
public class PostReviewer {
    /**
     * 标识
     */
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    /**
     * 审稿人id
     */
    @Column(name = "reviewer_uid")
    private Integer reviewerUid;

    /**
     * 稿件id
     */
    @Column(name = "pid")
    private Integer pid;

    /**
     * 审稿人是否接受审稿邀约
     */
    @Column(name = "accepted")
    private Integer accepted;

    /**
     * 审稿人对该稿件还能否审稿
     */
    @Column(name = "flag")
    private Boolean flag;


    /**
     * 创建时间
     */
    @Column(name = "create_time")
    private Date createTime;
}
