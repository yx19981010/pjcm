package com.samsph.pjcm.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.util.Date;

/**
 * 材料表实体类
 *
 * @author hujahao
 */
@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "material")
public class Material {
    /**
     * 标识
     */
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    /**
     * 材料名
     */
    @Column(name = "name")
    private String name;

    /**
     * 文件路径
     */
    @Column(name = "path")
    private String path;

    /**
     * 创建时间
     */
    @Column(name = "create_time")
    private Date createTime;

    /**
     * 创建者id
     */
    @Column(name = "create_by_uid")
    private Integer createByUid;

    /**
     * 材料描述
     */
    @Column(name = "description")
    private String content;
}
