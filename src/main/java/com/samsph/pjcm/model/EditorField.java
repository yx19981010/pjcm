package com.samsph.pjcm.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;

/**
 * 编辑-领域表实体类
 *
 * @author hujiahao
 */
@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "editor_field")
public class EditorField {
    /**
     * 标识
     */
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    /**
     * 编辑的用户id
     */
    @Column(name = "editor_uid")
    private Integer editorUid;

    /**
     * 领域
     */
    @Column(name = "field")
    private Integer field;
}
