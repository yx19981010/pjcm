package com.samsph.pjcm.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.util.Date;

/**
 * 证书表实体类
 * @author hujiahao
 */
@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "certificate")
public class Certificate {
    /**
     * 标识
     */
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name="id")
    private Integer id;

    /**
     * 证书名
     */
    @Column(name="name")
    private String name;

    /**
     * 证书描述
     */
    @Column(name="description")
    private String description;

    /**
     * 证书图片文件路径
     */
    @Column(name="path")
    private String path;

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
