package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

/**
 * 证书返回模型
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@ApiModel(description = "证书返回模型")
public class CertificateVoGet {
    /**
     * 证书id
     */
    @ApiModelProperty("证书id")
    private Integer id;

    /**
     * 证书名
     */
    @ApiModelProperty("证书名")
    private String name;

    /**
     * 证书描述
     */
    @ApiModelProperty("证书描述")
    private String description;

    /**
     * 证书图片文件路径
     */
    @ApiModelProperty("证书图片文件路径")
    private String path;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    private Date createTime;
}
