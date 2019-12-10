package com.samsph.pjcm.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.util.Date;

/**
 * 期刊表实体类
 *
 * @author hujiahao
 */
@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name="journal")
public class Journal {
    /**
     * 标识
     */
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;
    /**
     * 期刊文章数
     */
    @Column(name = "total")
    private Integer total;
    /**
     * 创建时间
     */
    @Column(name = "create_time")
    private Date createTime;
    /**
     * 年份
     */
    @Column(name = "year")
    private Integer year;
    /**
     * 月份
     */
    @Column(name = "month")
    private Integer month;
    /**
     * 卷号
     */
    @Column(name = "volume")
    private Integer volume;
    /**
     * 期号
     */
    @Column(name = "number")
    private Integer number;

    /**
     * 创建者id
     */
    @Column(name="create_by_uid")
    private Integer create_by_uid;
}