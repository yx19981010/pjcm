package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.constant.ErrMsg;
import com.samsph.pjcm.config.constant.MyBoolean;
import com.samsph.pjcm.config.constant.PostStatus;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.model.Post;
import com.samsph.pjcm.model.PostReviewer;
import com.samsph.pjcm.query.Add;
import com.samsph.pjcm.query.PostReviewerQuery;
import com.samsph.pjcm.query.Update;
import com.samsph.pjcm.service.PostReviewerService;
import com.samsph.pjcm.service.PostService;
import com.samsph.pjcm.service.UserRoleService;
import com.samsph.pjcm.vo.PostReviewerVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.dozer.Mapper;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.constraints.NotNull;

import java.util.List;

import static com.samsph.pjcm.config.DevUserId.EDITOR_ID;
import static com.samsph.pjcm.config.DevUserId.REVIEWER_ID;
import static com.samsph.pjcm.config.constant.RoleType.REVIEWER_ROLE;

/**
 * @author hujiahao
 */

@Slf4j
@Validated
@RestController
@RequestMapping("/api/v1/postReviewers")
@Api(tags = "3. 稿件审稿人管理")
public class PostReviewerController {

    @Resource
    private Mapper dozerMapper;

    @Resource
    PostService postService;

    @Resource
    UserRoleService userRoleService;

    @Resource
    PostReviewerService postReviewerService;

    @PostMapping()
    @PreAuthorize("hasAnyRole('ROLE_EDITOR')")
    @ApiOperation(value = "编辑为稿件选择审稿人（第一次选/前人拒审/审稿人转送）")
    public AjaxResponse addReviewer(@Validated({Add.class}) @RequestBody PostReviewerQuery postReviewerQuery) {
        int pid = postReviewerQuery.getPid();
        int reviewerUid = postReviewerQuery.getReviewerUid();

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = EDITOR_ID;

        // 检查操作者为该稿件编辑
        Post post = postService.getPost(pid);
        if (uid != post.getEditorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_EDITOR);
        }

        // 检查稿件状态
        if (post.getStatus() != PostStatus.REVIEWER_TO_BE_SELECTED.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        // 检查要添加的审搞人存在且角色正确
        if (userRoleService.findUserHasRole(reviewerUid, REVIEWER_ROLE)) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_REVIEWER);
        }

        // 保存该稿件-审稿人记录
        PostReviewer postReviewer = postReviewerService.save(postReviewerQuery);

        post.setStatus(PostStatus.FIRST_REVIEW.getCode());
        postService.updatePost(post);
        //TODO 给审稿人发邮件提示审稿
        return AjaxResponse.success(dozerMapper.map(postReviewer, PostReviewerVO.class));
    }

    @PutMapping()
    @PreAuthorize("hasAnyRole('ROLE_REVIEWER')")
    @ApiOperation(value = "审稿人接收/拒绝审稿")
    public AjaxResponse acceptOrRefuse(@Validated({Update.class}) @RequestBody PostReviewerQuery postReviewerQuery) {
        int pid = postReviewerQuery.getPid();
        boolean accept = postReviewerQuery.getAccept();

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = REVIEWER_ID;

        // 检查该审稿人未答复
        PostReviewer postReviewer = postReviewerService.getPostReviewer(pid, uid);
        if (postReviewer.getAccepted() != MyBoolean.DEFAULT.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.REVIEWER_HAS_REPLIED);
        }

        Post post = postService.getPost(pid);

        // 更新稿件-审稿人答复情况
        postReviewer.setAccepted(accept ? 1 : 0);
        postReviewerService.updatePostReviewer(postReviewer);

        // 更新稿件状态
        if (accept) {
            if (post.getCount() == 0) {
                // 第一次接收审稿
                post.setCount(1);
            }
        } else {
            post.setStatus(PostStatus.REVIEWER_TO_BE_SELECTED.getCode());
        }
        postService.updatePost(post);

        return AjaxResponse.success();
    }

    @DeleteMapping("/{id}")
    @ApiOperation("编辑取消稿件的审稿人")
    @PreAuthorize("hasAnyRole('ROLE_EDITOR')")
    public AjaxResponse deleteReviewer(@NotNull(message = "id不能为空") @PathVariable Integer id) {
        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = EDITOR_ID;
        // 检查操作者为该稿件编辑
        Post post = postService.getPost(id);
        if (uid != post.getEditorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_EDITOR);
        }
        postReviewerService.deletePostReviewer(id);
        return AjaxResponse.success();
    }
}
