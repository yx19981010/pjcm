package com.samsph.pjcm.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.bean.copier.CopyOptions;
import com.samsph.pjcm.config.PageData;
import com.samsph.pjcm.config.constant.*;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.dao.JournalRepository;
import com.samsph.pjcm.dao.PostRepository;
import com.samsph.pjcm.model.Journal;
import com.samsph.pjcm.model.Post;
import com.samsph.pjcm.query.PostReceiptQuery;
import com.samsph.pjcm.service.PostService;
import com.samsph.pjcm.config.utils.DozerUtil;
import com.samsph.pjcm.query.PostQuery;
import com.samsph.pjcm.vo.PostSimpleVO;
import com.samsph.pjcm.vo.PostVO;
import org.dozer.Mapper;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.Date;
import java.util.List;
import java.util.Optional;

/**
 * @author hujiahao
 */
@Service
public class PostServiceImpl implements PostService {
    @Resource
    private PostRepository postRepository;

    @Resource
    private JournalRepository journalRepository;

    @Resource
    private Mapper dozerMapper;

    private final static String NOT_FOUND = "投稿不存在";

    @Override
    public PostSimpleVO savePost(PostQuery postQuery, int creatorId) {
        Post post = dozerMapper.map(postQuery, Post.class);

        // 设置稿件创建时间
        post.setCreateTime(new Date());
        // 设置稿件审阅轮数
        post.setCount(0);
        // 设置稿件c初始稿件状态
        post.setStatus(PostStatus.TO_BE_SUBMITTED.getCode());
        // 设置稿件投稿人id
        post.setContributorUid(creatorId);

        post = postRepository.save(post);
        return dozerMapper.map(post, PostSimpleVO.class);
    }

    @Override
    public void setSubmitTime(int id) {
        Post post = fetchPost(id);
        post.setSubmitTime(new Date());
        postRepository.save(post);
    }

    @Override
    public void updateBasicInfo(PostQuery postQuery) {
        Post post = fetchPost(postQuery.getId());
        BeanUtil.copyProperties(postQuery, post, CopyOptions.create().setIgnoreNullValue(true).setIgnoreError(true));
        postRepository.save(post);
    }

    @Override
    public void updateReceiptInfo(PostReceiptQuery postReceiptQuery) {
        Post post = fetchPost(postReceiptQuery.getId());
        BeanUtil.copyProperties(postReceiptQuery, post);
        postRepository.save(post);
    }

    @Override
    public void checkPostInfo(Integer id) {
        Post post = fetchPost(id);

        String title = post.getTitle();
        Integer field = post.getField();
        Integer genreCode = post.getGenre();
        String writersInfo = post.getWritersInfo();
        Integer fundLevel = post.getFundLevel();
        String abstractZh = post.getAbstractZh();
        String abstractEn = post.getAbstractEn();
        String keywordsZh = post.getKeywordsZh();
        String keywordsEn = post.getAbstractEn();
        String postPath = post.getPostPath();
        String letterPath = post.getLetterPath();
        String ethicsApprovalPath = post.getEthicsApprovalPath();
        String fundApprovalPath = post.getFundApprovalPath();

        if (title == null || field == null || genreCode == null || writersInfo == null || fundLevel == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "标题、投稿领域、体裁、作者信息或基金级别等信息不完整，无法提交");
        }
        if (postPath == null || letterPath == null || ethicsApprovalPath == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "稿件、推荐信或伦理委员会批文未上传，无法提交");
        }
        if (fundLevel != FundLevel.NO.getCode() && fundApprovalPath == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "基金批文未上传，无法提交");
        }

        Genre genre = Genre.getItem(genreCode);
        if (genre == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "不支持的投稿体裁，无法提交");
        }
        checkGenre(genre, keywordsZh == null, keywordsEn == null, abstractZh == null, abstractEn == null);
    }

    @Override
    public void checkPaymentInfo(Integer id) {
        Post post = fetchPost(id);
        Boolean receiptedNeeded = post.getReceiptedNeeded();
        String taxpayerId = post.getTaxpayerId();
        String address = post.getReceiptAddress();
        String receiver = post.getReceiptReceiver();
        String invoice = post.getReceiptInvoice();
        String certificate = post.getCertificatePath();

        if (certificate == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "缴费证明未上传");
        }

        if (receiptedNeeded == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "缺少是否需要发票信息");
        } else if (receiptedNeeded) {
            if (taxpayerId == null || address == null || receiver == null || invoice == null) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "发票信息不完整");
            }
        }
    }

    @Override
    public void setEditor(int postId, int editorId) {
        Post post = fetchPost(postId);

        // TODO：检查是否存在该用户且角色为编辑

        // 设置稿件编辑
        post.setEditorUid(editorId);
        postRepository.save(post);
    }

    @Override
    public void setLayoutFee(int id, double fee) {
        Post post = fetchPost(id);
        post.setFee(fee);
        postRepository.save(post);
    }

    @Override
    public void setJournal(int id, int jid) {
        Post post = fetchPost(id);

        // 检查是否存在该期刊
        Optional<Journal> journalOptional = journalRepository.findById(jid);
        if (!journalOptional.isPresent()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "期刊不存在");
        }

        post.setJid(jid);
        postRepository.save(post);
    }

    @Override
    public void checkStatus(int id, PostStatus postStatus) {
        Post post = fetchPost(id);

        if (post.getStatus() == postStatus.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "当前投稿状态错误");
        }
    }

    @Override
    public void updateStatus(int id, PostStatus postStatus) {
        Post post = fetchPost(id);
        post.setStatus(postStatus.getCode());
        postRepository.save(post);
    }

    @Override
    public void checkAndUpdateStatus(int id, PostStatus oldStatus, PostStatus newStatus) {
        Post post = fetchPost(id);

        if (post.getStatus() == oldStatus.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "当前投稿状态错误");
        }

        post.setStatus(newStatus.getCode());
        postRepository.save(post);
    }


    @Override
    public void rejectPost(int id, EditorRejectType type, String comment) {
        Post post = fetchPost(id);

        switch (type) {
            case FIRST_EXAM_REJECT:
                post.setFirstExamComment(comment);
                post.setFirstExamCommentTime(new Date());
                post.setStatus(PostStatus.FIRST_EXAM_REJECTED.getCode());
                break;
            case EDITOR_REJECT:
                post.setRejectComment(comment);
                post.setRejectCommentTime(new Date());
                post.setStatus(PostStatus.EDITOR_REJECT.getCode());
                break;
            case FORMAT_REJECT:
                post.setFormatComment(comment);
                post.setFormatCommentTime(new Date());
                post.setStatus(PostStatus.FORMAT_TO_BE_MODIFIED.getCode());
                break;
            case PAYMENT_REJECT:
                post.setCertificateComment(comment);
                post.setCertificateCommentTime(new Date());
                post.setStatus(PostStatus.CERTIFICATE_TO_BE_UPLOADED.getCode());
                break;
            default:
                throw new CustomException(CustomExceptionType.OTHER_ERROR, "未知拒绝类型");
        }
        postRepository.save(post);
    }

    @Override
    public PostVO getPost(int id) {
        Post post = fetchPost(id);

        return dozerMapper.map(post, PostVO.class);
    }

    @Override
    public PageData getAll(int page, int size, String sort, boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(page, size, direction, sort);

        // 查询分页结果
        Page<Post> postPage = postRepository.findAll(pageRequest);

        // 封装分页数据并返回
        return encapsulatePageData(postPage, page);
    }

    @Override
    public PageData getAllByStatus(PostStatus status, int page, int size, String sort, boolean ascend) {
        // 新建分页选项
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(page, size, direction, sort);

        // 查询分页结果
        Page<Post> postPage = postRepository.findAllByStatus(status.getCode(), pageRequest);

        // 封装分页数据并返回
        return encapsulatePageData(postPage, page);
    }

    @Override
    public void deletePost(int id) {
        Post post = fetchPost(id);

        // 删除投稿
        postRepository.deleteById(id);
    }

    private Post fetchPost(int id) {
        // 查询是否存在该id的投稿
        Optional<Post> postOptional = postRepository.findById(id);

        // 若不存在则抛出异常
        if (!postOptional.isPresent()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, NOT_FOUND);
        }

        return postOptional.get();
    }

    private void checkGenre(Genre genre, boolean hasZhKw, boolean hasEnKw, boolean hasZhAbstract, boolean hasEnAbstract) {
        switch (genre) {
            case WORKS:
                if (!(hasZhKw && hasEnKw && hasZhAbstract && hasEnAbstract)) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "论著体裁需中英文摘要和关键字");
                }
                break;
            case OVERVIEW:
                if (!(hasZhKw && hasZhAbstract)) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "综述体裁需中文摘要和关键字");
                }
                break;

            case PAPER:
                if (!(hasZhKw && hasZhAbstract)) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "论文体裁只需中文摘要和关键字");
                }
                break;
            case CASE:
                break;
            default:
                throw new CustomException(CustomExceptionType.OTHER_ERROR, "未知体裁");
        }
    }

    private PageData encapsulatePageData(Page<Post> postPage, int at) {
        int totalPages = postPage.getTotalPages();
        int totalElements = (int) postPage.getTotalElements();
        List<Post> postLis = postPage.getContent();
        int cnt = postLis.size();
        return new PageData(totalPages, totalElements, at, cnt, DozerUtil.mapList(postLis, PostSimpleVO.class));
    }
}
