package com.samsph.pjcm.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;

/**
 * 稿件表实体类
 *
 * @author hujahao
 */
@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "post_no_offset")
public class PostNoOffset {
    /**
     * 标识
     */
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    /**
     * 年号
     */
    @Column(name = "year")
    private Integer year;

    /**
     * 偏移值，即该年已经使用的稿件编号
     */
    @Column(name = "offset")
    private Integer offset;
}
