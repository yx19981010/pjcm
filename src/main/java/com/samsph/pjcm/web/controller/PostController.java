package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.constant.EditorRejectType;
import com.samsph.pjcm.config.constant.PostStatus;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.query.Add;
import com.samsph.pjcm.query.Update;
import com.samsph.pjcm.query.Update2;
import com.samsph.pjcm.query.*;
import com.samsph.pjcm.service.PostService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.constraints.NotNull;


/**
 * @author hujiahao
 */

@Slf4j
@RestController
@RequestMapping("/api/v1/posts")
@Validated
@Api(tags = "2. 投稿管理")
public class PostController {
    @Resource(name = "postServiceImpl")
    PostService postService;

    @PostMapping()
    @ApiOperation(value = "投稿人创建一次投稿")
    public AjaxResponse savePost(@Validated({Add.class}) @RequestBody PostQuery postQuery) {
        // TODO：以某种方式获得当前操作用户，检查用户角色是否为投稿人

        log.info("saveJournal: {}", postQuery);

        // TODO: 以某种方式获得投稿人id，此处把投稿人id先写死为8
        return AjaxResponse.success(postService.savePost(postQuery, 8));
    }

    @PutMapping("type=1")
    @ApiOperation(value = "稿件状态为待提交，投稿人修改投稿基本信息")
    public AjaxResponse updatePost1(@Validated({Update.class}) @RequestBody PostQuery postQuery) {
        // TODO: 以某种方式获得当前操作用户，检查其是否为该稿件的投稿人

        postService.updateBasicInfo(postQuery);
        return AjaxResponse.success();
    }

    @PutMapping("type=2")
    @ApiOperation(value = "稿件状态为待提交，投稿人提交以待初审")
    public AjaxResponse updatePost2(@NotNull @RequestBody Integer id) {
        // TODO: 以某种方式获得当前操作用户，检查其是否为该稿件的投稿人

        // 检查字段是否填写齐全、文件是否上传完整
        postService.checkPostInfo(id);

        // TODO：以某种方式获得编辑id，此处把编辑id先写死为8
        postService.setEditor(id, 8);

        postService.checkAndUpdateStatus(id, PostStatus.TO_BE_SUBMITTED, PostStatus.PENDING_FIRST_EXAM);
        return AjaxResponse.success();
    }

    @PutMapping("type=3")
    @ApiOperation(value = "稿件状态为待初审，编辑进行初审")
    public AjaxResponse updatePost3(@Validated @RequestBody EditorAuditQuery editorAuditQuery) {
        // TODO: 以某种方式获得当前操作用户，检查其是否为该稿件的编辑

        int id = editorAuditQuery.getId();
        boolean passed = editorAuditQuery.getPassed();
        String comment = editorAuditQuery.getComment();

        postService.checkStatus(id, PostStatus.PENDING_FIRST_EXAM);

        if (passed) {
            postService.setSubmitTime(id);
            postService.updateStatus(id, PostStatus.REVIEWER_TO_BE_SELECTED);
        } else {
            if (comment == null) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "初审否决需填写意见");
            }
            postService.rejectPost(id, EditorRejectType.FIRST_EXAM_REJECT, comment);
        }

        return AjaxResponse.success();
    }

    @PutMapping("type=4")
    @ApiOperation(value = "稿件状态为待退回，编辑选择退改或退稿")
    public AjaxResponse updatePost4(@Validated @RequestBody EditorAuditQuery editorAuditQuery) {
        // TODO: 以某种方式获得当前操作用户，检查其是否为该稿件的编辑

        int id = editorAuditQuery.getId();
        boolean passed = editorAuditQuery.getPassed();
        String comment = editorAuditQuery.getComment();

        postService.checkStatus(id, PostStatus.TO_BE_RETURNED);

        if (passed) {
            postService.updateStatus(id, PostStatus.TO_BE_REVISED);
        } else {
            if (comment == null) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "退稿需填写意见");
            }
            // 编辑退稿
            postService.rejectPost(id, EditorRejectType.EDITOR_REJECT, comment);
        }

        return AjaxResponse.success();
    }

    @PutMapping("type=5")
    @ApiOperation(value = "稿件状态为待修改，投稿人修改稿件基本信息")
    public AjaxResponse updatePost5(@Validated({Update2.class}) @RequestBody PostQuery postQuery) {
        // TODO: 以某种方式获得当前操作用户，检查其是否为该稿件的投稿人

        postService.updateBasicInfo(postQuery);
        return AjaxResponse.success();
    }

    @PutMapping("type=6")
    @ApiOperation(value = "稿件状态为待修改，投稿人提交待再审")
    public AjaxResponse updatePost6(@NotNull @RequestBody Integer id) {
        // TODO: 以某种方式获得当前操作用户，检查其是否为该稿件的投稿人

        postService.checkAndUpdateStatus(id, PostStatus.TO_BE_REVISED, PostStatus.RE_REVIEW);
        return AjaxResponse.success();
    }

    @PutMapping("type=7")
    @ApiOperation(value = "稿件状态为格式待审核，编辑进行格式审核")
    public AjaxResponse updatePost7(@Validated @RequestBody EditorAuditQuery editorAuditQuery) {
        // TODO: 以某种方式获得当前操作用户，检查其是否为该稿件的编辑

        int id = editorAuditQuery.getId();
        boolean passed = editorAuditQuery.getPassed();
        String comment = editorAuditQuery.getComment();

        postService.checkStatus(id, PostStatus.FORMAT_TO_BE_REVIEWED);

        if (passed) {
            postService.updateStatus(id, PostStatus.LAYOUT_FEE_TO_BE_DETERMINED);
        } else {
            if (comment == null) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "格式不通过需填写意见");
            }
            // 格式审核不通过
            postService.rejectPost(id, EditorRejectType.FORMAT_REJECT, comment);
        }
        return AjaxResponse.success();
    }

    @PutMapping("type=8")
    @ApiOperation(value = "稿件状态为格式待修改，投稿人提交待格式审核")
    public AjaxResponse updatePost8(@Validated @RequestBody Integer id) {
        // TODO: 以某种方式获得当前操作用户，检查其是否为该稿件的投稿人

        postService.checkAndUpdateStatus(id, PostStatus.FORMAT_TO_BE_MODIFIED, PostStatus.FORMAT_TO_BE_REVIEWED);
        return AjaxResponse.success();
    }

    @PutMapping("type=9")
    @ApiOperation(value = "稿件状态为待确定版面费，编辑确定版面费")
    public AjaxResponse updatePost9(@Validated @RequestBody PostLayOutFeeQuery postLayOutFeeQuery) {
        // TODO: 以某种方式获得当前操作用户，检查其是否为该稿件的编辑

        int id = postLayOutFeeQuery.getId();
        // 检查状态为待确定版面费
        postService.checkStatus(id, PostStatus.LAYOUT_FEE_TO_BE_DETERMINED);
        // 设置版面费
        postService.setLayoutFee(id, postLayOutFeeQuery.getFee());
        // 更新状态为待上传缴费证明
        postService.updateStatus(id, PostStatus.CERTIFICATE_TO_BE_UPLOADED);
        return AjaxResponse.success();
    }

    @PutMapping("type=10")
    @ApiOperation(value = "稿件状态为缴费证明待上传，投稿人填写收据信息")
    public AjaxResponse updatePost10(@Validated @RequestBody PostReceiptQuery postReceiptQuery) {
        // TODO: 以某种方式获得当前操作用户，检查其是否为该稿件的投稿人

        postService.checkStatus(postReceiptQuery.getId(), PostStatus.CERTIFICATE_TO_BE_UPLOADED);
        postService.updateReceiptInfo(postReceiptQuery);
        return AjaxResponse.success();
    }

    @PutMapping("type=11")
    @ApiOperation(value = "稿件状态为缴费证明待上传，投稿人确认提交")
    public AjaxResponse updatePost11(@NotNull @RequestBody Integer id) {
        // TODO: 以某种方式获得当前操作用户，检查其是否为该稿件的投稿人

        postService.checkStatus(id, PostStatus.CERTIFICATE_TO_BE_UPLOADED);
        // 检查收据信息、缴费证明是否完整
        postService.checkPaymentInfo(id);

        postService.updateStatus(id, PostStatus.PAYMENT_TO_BE_EXAMINED);
        return AjaxResponse.success();
    }

    @PutMapping("type=12")
    @ApiOperation(value = "稿件状态为缴费证明待审核，编辑进行审核")
    public AjaxResponse updatePost12(@Validated @RequestBody EditorAuditQuery editorAuditQuery) {
        // TODO: 以某种方式获得当前操作用户，检查其是否为该稿件的编辑

        int id = editorAuditQuery.getId();
        boolean passed = editorAuditQuery.getPassed();
        String comment = editorAuditQuery.getComment();

        postService.checkStatus(id, PostStatus.PAYMENT_TO_BE_EXAMINED);

        if (passed) {
            postService.updateStatus(id, PostStatus.SUCCESS);
        } else {
            if (comment == null) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "缴费审核不通过需填写意见");
            }
            // 缴费审核未通过
            postService.rejectPost(id, EditorRejectType.PAYMENT_REJECT, comment);
        }
        return AjaxResponse.success();
    }

    @PutMapping("type=13")
    @ApiOperation(value = "稿件状态为投稿成功，编辑将稿件加入到某一期期刊")
    public AjaxResponse updatePost13(@Validated @RequestBody PostJournalQuery postJournalQuery) {
        // TODO: 以某种方式获得当前操作用户，检查其是否为该稿件的编辑

        int id = postJournalQuery.getId();
        postService.checkStatus(id, PostStatus.SUCCESS);
        postService.setJournal(id, postJournalQuery.getJid());
        return AjaxResponse.success();
    }

    // TODO: get 按role/status/列表、详情；
//    @GetMapping("type=1")
//    @ApiOperation(value="编辑获取稿件列表")
//    AjaxResponse getPosts(@Validated @PathVariable )

    @ApiOperation(value = "根据id删除期刊记录")
    @DeleteMapping("/{id}")
    public AjaxResponse deletePost(@NotNull(message = "id不能为空") @PathVariable Integer id) {
        // TODO: 检查用户是否为管理员
        log.info("deletePost: {}", id);
        postService.deletePost(id);
        return AjaxResponse.success();
    }
}
