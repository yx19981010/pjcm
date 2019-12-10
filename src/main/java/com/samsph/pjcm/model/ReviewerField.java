package com.samsph.pjcm.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;

/**
 * 审稿人-领域表实体类
 *
 * @author hujiahao
 */
@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "reviewer_field")
public class ReviewerField {
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
     * 领域
     */
    @Column(name = "field")
    private Integer field;
}
