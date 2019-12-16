package com.samsph.pjcm.config;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;

import java.util.List;

/**
 * @author hujiahao
 */
@Data
@AllArgsConstructor
@ApiModel(description="分页数据实体类")
public class PageDataT<T> {
    @ApiModelProperty("总页数")
    int totalPages;

    @ApiModelProperty("总条目数")
    int totalItems;

    @ApiModelProperty("当前是第几页")
    int number;

    @ApiModelProperty("该页有多少条目")
    int count;

    @ApiModelProperty("分页列表")
    List<T> list;
}
