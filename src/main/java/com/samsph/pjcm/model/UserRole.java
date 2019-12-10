package com.samsph.pjcm.model;


import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;

/**
 * 用户角色表实体类
 *
 * @author hujahao
 */
@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "user_role")
public class UserRole {
    /**
     * 标识
     */
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Integer id;

    /**
     * 用户id
     */
    @Column(name = "uid")
    private Integer uid;

    /**
     * 角色
     */
    @Column(name = "role")
    private Integer role;
}
