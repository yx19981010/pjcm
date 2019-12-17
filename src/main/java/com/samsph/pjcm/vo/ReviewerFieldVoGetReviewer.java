package com.samsph.pjcm.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ReviewerFieldVoGetReviewer {
    /**
     * 标识
     */
    private Integer id;

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
}
