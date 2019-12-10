package com.samsph.pjcm.config;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.AllArgsConstructor;
import lombok.Data;

import java.util.List;

/**
 * 分页数据类，包含了分页列表和基本分页信息
 *
 * @author hujahao
 */

@ApiModel(description="分页数据实体类")
@Data
@AllArgsConstructor
public class PageData {
    @ApiModelProperty("总页数")
    int totalPages;

    @ApiModelProperty("总条目数")
    int totalItems;

    @ApiModelProperty("当前是第几页")
    int number;

    @ApiModelProperty("该页有多少条目")
    int count;

    @ApiModelProperty("分页列表")
    List list;
}
