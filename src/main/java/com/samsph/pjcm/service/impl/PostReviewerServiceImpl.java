package com.samsph.pjcm.service.impl;

import com.samsph.pjcm.config.constant.ErrMsg;
import com.samsph.pjcm.config.constant.MyBoolean;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.dao.PostReviewerRepository;
import com.samsph.pjcm.model.PostReviewer;
import com.samsph.pjcm.query.PostReviewerQuery;
import com.samsph.pjcm.service.PostReviewerService;
import org.dozer.Mapper;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.Date;
import java.util.List;
import java.util.Optional;

/**
 * @author hujiahao
 */
@Service
public class PostReviewerServiceImpl implements PostReviewerService {
    @Resource
    private PostReviewerRepository postReviewerRepository;

    @Resource
    private Mapper dozerMapper;

    @Override
    public PostReviewer save(PostReviewerQuery postReviewerQuery) {
        int uid = postReviewerQuery.getReviewerUid();
        int pid = postReviewerQuery.getPid();

        Optional<PostReviewer> postReviewerOptional = postReviewerRepository.findByPidAndReviewerUid(pid, uid);
        if (postReviewerOptional.isPresent()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.POST_REVIEWER_EXISTS);
        }

        PostReviewer postReviewer = dozerMapper.map(postReviewerQuery, PostReviewer.class);

        // 设置稿件审稿标识、创建时间、接受状态
        postReviewer.setFlag(false);
        postReviewer.setCreateTime(new Date());
        postReviewer.setAccepted(MyBoolean.DEFAULT.getCode());

        return postReviewerRepository.save(postReviewer);
    }

    @Override
    public PostReviewer getPostReviewer(int pid, int uid) {
        return fetchPostReviewer(pid, uid);
    }

    @Override
    public void updatePostReviewer(PostReviewer postReviewer) {
        postReviewerRepository.save(postReviewer);
    }

    @Override
    public boolean aggregate(int pid) {
        // 获得下轮审将进行审稿（即上轮审稿意见为“待修改”）的稿件-审稿人记录列表
        List<PostReviewer> postReviewers = postReviewerRepository.findByPidAndFlag(pid, true);

        // 如果为空，代表通过；否则为建议修改
        return postReviewers.isEmpty();
    }

    @Override
    public void deletePostReviewer(int id) {
        Optional<PostReviewer> postReviewerOptional = postReviewerRepository.findById(id);

        // 若不存在则抛出异常
        if (postReviewerOptional.isEmpty()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.POST_REVIEWER_NOT_FOUND);
        }

        postReviewerRepository.deleteById(id);
    }

    private PostReviewer fetchPostReviewer(int pid, int uid) {
        Optional<PostReviewer> postReviewerOptional = postReviewerRepository.findByPidAndReviewerUid(pid, uid);

        // 若不存在则抛出异常
        if (postReviewerOptional.isEmpty()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.POST_REVIEWER_NOT_FOUND);
        }

        return postReviewerOptional.get();
    }
}
