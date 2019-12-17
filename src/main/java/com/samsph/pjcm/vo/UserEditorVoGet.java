package com.samsph.pjcm.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UserEditorVoGet {
    /**
     * 标识
     */
    private Integer id;

    /**
     * 用户名
     */
    private String userName;

    /**
     * 电子邮件
     */
    private String email;

    /**
     * 性别
     */
    private Integer sex;

    /**
     * 电话
     */
    private String phone;

    /**
     * 领域
     */
    private List<EditorFieldVoGetField> field;

    /**
     * 状态(-1注销，0未激活，1激活)
     */
    private Integer active;


}
