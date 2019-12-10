package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.query.PostReviewerQuery;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

/**
 * @author hujiahao
 */

@Slf4j
@RestController
@RequestMapping("/api/v1/postReviewers")
@Api(tags = "3. 稿件审稿人管理")
public class PostReviewerController {
    @PostMapping()
    @ApiOperation(value = "编辑为稿件选择审稿人")
    public AjaxResponse addReviewer(@RequestBody PostReviewerQuery postReviewerQuery) {
        // TODO: 编辑为稿件选择审稿人
        // 编辑第一次根据投稿领域选择审稿人
        // 审稿人拒绝审稿邀约后编辑重新进行选择
        // 编辑根据首轮审稿中转送意见选择审稿人
        return AjaxResponse.success();
    }

    @PutMapping()
    @ApiOperation(value = "审稿人接收/拒绝审稿")
    public AjaxResponse acceptOrRefuse() {
        return AjaxResponse.success();
    }

    @GetMapping("type=1")
    @ApiOperation(value = "编辑获取审稿接受情况")
    public AjaxResponse getAll() {
        return AjaxResponse.success();
    }

    @GetMapping("type=2")
    @ApiOperation(value = "审稿人获取相关稿件")
    public AjaxResponse getAll2() {
        return AjaxResponse.success();
    }

    @DeleteMapping("/{id}")
    @ApiOperation("编辑为稿件取消审稿人")
    public AjaxResponse deleteReviewer() {
        return AjaxResponse.success();
    }
}
