package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.util.Date;

/**
 * 公告返回模型
 */
@AllArgsConstructor
@NoArgsConstructor
@Data
@ApiModel(description = "公告返回实体")
public class AnnouncementVoGet {
    /**
     * 公告id
     */
    @ApiModelProperty("公告id")
    private Integer id;
    /**
     * 公告标题
     */
    @ApiModelProperty("公告标题")
    private String title;
    /**
     * 公告描述
     */
    @ApiModelProperty("公告描述")
    private String content;
    /**
     * 公告创建时间
     */
    @ApiModelProperty("公告创建时间")
    private Date createTime;
}
