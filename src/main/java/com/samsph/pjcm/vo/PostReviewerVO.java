package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import lombok.Data;

import java.util.Date;

/**
 * @author hujiahao
 */

@Data
@ApiModel(description = "投稿-审稿人实体类")
public class PostReviewerVO {
    private Integer id;

    private Integer reviewerUid;

    private Integer pid;

    private Boolean accepted;

    private Date createTime;
}
