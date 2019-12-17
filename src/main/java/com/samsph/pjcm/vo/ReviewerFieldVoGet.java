package com.samsph.pjcm.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ReviewerFieldVoGet{
    /**
     * 审稿人id
     */
    private Integer reviewerUid;

    /**
     * 用户名
     */
    private String userName;

    /**
     * 电子邮件
     */
    private String email;

    /**
     * 领域
     */
    private List<ReviewerFieldVoGetField> field;

}
