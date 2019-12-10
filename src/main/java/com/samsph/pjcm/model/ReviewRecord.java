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
     * 审稿人-稿件id
     */
    @Column(name="r_pid")
    private Integer rPid;

    /**
     * 审稿结果
     */
    @Column(name="result")
    private Integer result;

    /**
     * 转送类型
     */
    @Column(name="forward_type")
    private Integer forwardType;

    /**
     * 转送id（领域或者审稿人）
     */
    @Column(name="forward_to")
    private Integer forwardTo;

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
}
