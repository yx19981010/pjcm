package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.constant.ErrMsg;
import com.samsph.pjcm.config.constant.PostStatus;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.model.Post;
import com.samsph.pjcm.model.PostReviewer;
import com.samsph.pjcm.model.ReviewRecord;
import com.samsph.pjcm.query.Add;
import com.samsph.pjcm.query.Add2;
import com.samsph.pjcm.query.ReviewRecordQuery;
import com.samsph.pjcm.service.PostReviewerService;
import com.samsph.pjcm.service.PostService;
import com.samsph.pjcm.service.ReviewRecordService;
import com.samsph.pjcm.vo.ReviewRecordVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.dozer.Mapper;
import org.springframework.data.domain.Page;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.constraints.NotNull;

import static com.samsph.pjcm.config.DevUserId.EDITOR_ID;
import static com.samsph.pjcm.config.DevUserId.REVIEWER_ID;

/**
 * @author hujiahao
 */
@Slf4j
@Validated
@RestController
@RequestMapping("/api/v1/reviewRecords")
@Api(tags = "4. 审稿记录管理")
public class ReviewRecordController {

    @Resource
    private Mapper dozerMapper;

    @Resource
    PostService postService;

    @Resource
    PostReviewerService postReviewerService;

    @Resource
    ReviewRecordService reviewRecordService;

    @PostMapping("type=1")
    @ApiOperation(value = "首轮审稿可以否决和转送")
    public AjaxResponse review(@Validated({Add.class}) @RequestBody ReviewRecordQuery reviewRecordQuery) {
        int pid = reviewRecordQuery.getPid();
        Boolean reject = reviewRecordQuery.getReject();
        String rejectComment = reviewRecordQuery.getRejectComment();
        Boolean toForward = reviewRecordQuery.getToForward();
        String forwardComment = reviewRecordQuery.getForwardComment();
        Boolean toRevise = reviewRecordQuery.getToRevise();
        String reviseComment = reviewRecordQuery.getReviseComment();

        // 检查字段填写完整
        if (reject) {
            if (rejectComment == null || rejectComment.isBlank()) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.REJECT_COMMENT_NEEDED);
            }
            if (toForward || toRevise || !forwardComment.isBlank() || reviseComment.isBlank()) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.CANNOT_REVISE_OR_FORWARD);
            }
        } else {
            if (toForward) {
                if (forwardComment == null || forwardComment.isBlank()) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.FORWARD_COMMENT_NEEDED);
                }
            }
            if (toRevise) {
                if (reviseComment == null || reviseComment.isBlank()) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.REVISE_COMMENT_NEEDED);
                }
            }
        }

        // TODO：获取当前操作用户id
        int uid = REVIEWER_ID;

        // 检查稿件状态
        Post post = postService.getPost(pid);
        if (post.getStatus() != PostStatus.FIRST_REVIEW.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        // 检查当前用户能否审稿
        PostReviewer postReviewer = postReviewerService.getPostReviewer(pid, uid);
        if (!postReviewer.getFlag()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.CANNOT_REVIEW);
        }

        // 保存审稿记录
        ReviewRecord reviewRecord = reviewRecordService.save(reviewRecordQuery, uid, 1);

        if (reject) {
            // 审稿人否决，更新稿件状态
            post.setStatus(PostStatus.REVIEWER_REJECT.getCode());
        } else {
            // 审稿人没有否决
            if (!toRevise) {
                // 没有建议修改即通过，更新审稿标识（下轮不用再审稿）
                postReviewer.setFlag(false);
                postReviewerService.updatePostReviewer(postReviewer);
            }
            if (toForward) {
                // 转送
                post.setStatus(PostStatus.REVIEWER_TO_BE_SELECTED.getCode());
            } else {
                // 不否决不转送，第一轮审稿结束，汇总该轮审稿结果
                if (postReviewerService.aggregate(pid)) {
                    // 汇总结果为通过
                    post.setStatus(PostStatus.FORMAT_TO_BE_REVIEWED.getCode());
                } else {
                    // 汇总结果为建议修改
                    post.setStatus(PostStatus.TO_BE_RETURNED.getCode());
                }
            }
        }
        postService.updatePost(post);

        return AjaxResponse.success(dozerMapper.map(reviewRecord, ReviewRecordVO.class));
    }


    @PostMapping("type=2")
    @ApiOperation(value = "再审稿只能通过或建议修改，不能否决或转送")
    public AjaxResponse review2(@Validated({Add2.class}) @RequestBody ReviewRecordQuery reviewRecordQuery) {
        int pid = reviewRecordQuery.getPid();
        Boolean toRevise = reviewRecordQuery.getToRevise();
        String reviseComment = reviewRecordQuery.getReviseComment();

        // 检查字段填写是否完整
        if (toRevise) {
            if (reviseComment == null || reviseComment.isBlank()) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.REVISE_COMMENT_NEEDED);
            }
        }

        // TODO：获取当前操作用户id
        int uid = REVIEWER_ID;

        // 检查稿件状态
        Post post = postService.getPost(pid);
        if (post.getStatus() != PostStatus.RE_REVIEW.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        // 检查当前用户能否审稿
        PostReviewer postReviewer = postReviewerService.getPostReviewer(pid, uid);
        if (!postReviewer.getFlag()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.CANNOT_REVIEW);
        }

        // 保存审稿记录
        ReviewRecord reviewRecord = reviewRecordService.save(reviewRecordQuery, uid, post.getCount());

        // 审稿人没有建议修改，即通过
        if (!toRevise) {
            // 没有建议修改即通过，更新审稿标识（下轮不用再审稿）
            postReviewer.setFlag(false);
            postReviewerService.updatePostReviewer(postReviewer);
        }

        if (reviewRecordService.canReReviewClose(pid)) {
            // 此轮审稿可关闭，则汇总该轮审稿结果
            if (postReviewerService.aggregate(pid)) {
                // 汇总结果为通过
                post.setStatus(PostStatus.FORMAT_TO_BE_REVIEWED.getCode());
            } else {
                // 汇总结果为建议修改
                post.setStatus(PostStatus.TO_BE_RETURNED.getCode());
            }
            postService.updatePost(post);
        }
        return AjaxResponse.success(dozerMapper.map(reviewRecord, ReviewRecordVO.class));
    }

    @GetMapping("/{pid}/type=1")
    @ApiOperation(value = "编辑根据pid获得某一稿件的审稿记录")
    public AjaxResponse getAll1(@NotNull(message = "id不能为空") @PathVariable Integer pid,
                                @NotNull(message = "number不能为空") @RequestParam("number") Integer number,
                                @NotNull(message = "size不能为空") @RequestParam("size") Integer size,
                                @RequestParam(value = "ascend", required = false) Boolean ascend) {
        // TODO: 以某种方式获得当前操作用户id
        int uid = EDITOR_ID;

        // 检查操作者为该稿件编辑
        Post post = postService.getPost(pid);
        if (uid != post.getEditorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_EDITOR);
        }

        if (ascend == null) {
            ascend = true;
        }

        Page<ReviewRecord> page = reviewRecordService.getAllByPid(pid, number, size, ascend);

        return AjaxResponse.success();
    }

    @GetMapping("/{pid}/type=2")
    @ApiOperation(value = "投稿人根据id获得某一稿件的审稿记录")
    public AjaxResponse getAll2() {
        return null;
    }

    @GetMapping("/{pid}/type=3")
    @ApiOperation(value = "审稿人根据id获得自己对某一稿件的审稿记录")
    public AjaxResponse getAll3() {
        return null;
    }
}
