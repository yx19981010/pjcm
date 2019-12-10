package com.samsph.pjcm.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.util.Date;

/**
 * 公告表实体类
 * @author hujiahao
 */
@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "announcement")
public class Announcement {
    /**
     * 标识
     */
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name="id")
    private Integer id;
    /**
     * 公告标题
     */
    @Column(name="title")
    private String title;
    /**
     * 公告内容
     */
    @Column(name="content")
    private String content;
    /**
     * 创建时间
     */
    @Column(name="create_time")
    private Date createTime;
    /**
     * 创建者id
     */
    @Column(name="create_by_uid")
    private Integer createByUid;
}