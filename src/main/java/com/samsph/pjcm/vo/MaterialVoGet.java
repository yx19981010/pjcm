package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

/**
 * 材料返回模型
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@ApiModel(description = "材料返回模型")
public class MaterialVoGet {
    /**
     * 材料id
     */
    @ApiModelProperty("材料id")
    private Integer id;

    /**
     * 材料名
     */
    @ApiModelProperty("材料名")
    private String name;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    private Date createTime;

    /**
     * 材料描述
     */
    @ApiModelProperty("材料描述")
    private String content;
}
