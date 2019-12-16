package com.samsph.pjcm.service;

import com.samsph.pjcm.config.constant.MyBoolean;
import com.samsph.pjcm.config.constant.PostStatus;
import com.samsph.pjcm.model.Post;
import com.samsph.pjcm.query.PostQuery;
import org.springframework.data.domain.Page;

import java.util.Date;
import java.util.List;

/**
 * 稿件表service层接口
 *
 * @author hujiahao
 */
public interface PostService {
    /**
     * 新增一条稿件记录
     *
     * @param postQuery 稿件请求
     * @param creatorId 投稿人id
     * @return 稿件实体
     */
    Post savePost(PostQuery postQuery, int creatorId);

    /**
     * 根据id获取稿件
     *
     * @param id 稿件id
     * @return 稿件实体对象
     */
    Post getPost(int id);
    
    /**
     * 根据投稿人id获取其稿件分页
     *
     * @param uid    投稿人id
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByCtrUid(int uid, int number, int size, boolean ascend);

    /**
     * 根据投稿人id获取某段时间内提交的稿件分页
     *
     * @param uid    投稿人id
     * @param start  开始时间
     * @param end    结束时间
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 稿件分页
     */
    public Page<Post> getAllByCtrUidAndSubmitTime(int uid, Date start, Date end, int number, int size, boolean ascend);

    /**
     * 根据投稿人id和稿件状态获取稿件分页
     *
     * @param uid    投稿人id
     * @param status 稿件状态
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByCtrUidAndStatus(int uid, PostStatus status, int number, int size, boolean ascend);


    /**
     * 根据投稿人id和稿件状态获取某段时间内提交的稿件分页
     *
     * @param uid    投稿人id
     * @param status 状态
     * @param start  开始时间
     * @param end    结束时间
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByCtrUidAndStatusAndSubmitTime(int uid, PostStatus status, Date start, Date end, int number, int size, boolean ascend);

    /**
     * 根据编辑id获取其负责编辑的稿件分页
     *
     * @param uid    编辑id
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByEdUid(int uid, int number, int size, boolean ascend);


    /**
     * 根据编辑id获取其负责编辑的某段时间内提交的稿件分页
     *
     * @param uid    编辑id
     * @param start  开始时间
     * @param end    结束时间
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByEdUidAndSubmitTime(int uid, Date start, Date end, int number, int size, boolean ascend);


    /**
     * 根据编辑id和稿件状态获取其负责编辑的稿件分页
     *
     * @param uid    编辑id
     * @param status 稿件状态
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByEdUidAndStatus(int uid, PostStatus status, int number, int size, boolean ascend);

    /**
     * 根据编辑id和稿件状态获取其负责编辑的某段时间内提交的稿件分页
     *
     * @param uid    编辑id
     * @param status 稿件状态
     * @param start  开始时间
     * @param end    结束时间
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByEdUidAndStatusAndSubmitTime(int uid, PostStatus status, Date start, Date end, int number, int size, boolean ascend);

    /**
     * 根据审稿人id和接受审稿标识获取审稿人相关的的稿件分页
     *
     * @param uid    编辑id
     * @param accept 接受审稿标识
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByRevUidAndAccept(int uid, MyBoolean accept, int number, int size, boolean ascend);


    /**
     * 根据审稿人id和接受审稿标识获取审稿人相关的的某段时间内提交的稿件分页
     *
     * @param uid    审稿人id
     * @param accept 接受审稿标识
     * @param start  开始时间
     * @param end    结束时间
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByRevUidAndAcceptAndSubmitTime(int uid, MyBoolean accept, Date start, Date end, int number, int size, boolean ascend);

    /**
     * 根据审稿人id和审稿标识获取审稿人需要审稿的稿件分页
     *
     * @param uid    审稿人id
     * @param flag   是否需要审稿
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByRevUidAndFlag(int uid, boolean flag, int number, int size, boolean ascend);

    /**
     * 根据审稿人id和审稿标识获取审稿人已接受的某段时间内提交的稿件分页
     *
     * @param uid    审稿人id
     * @param flag   是否需要审稿
     * @param number 页号
     * @param size   页面大小
     * @param start  开始时间
     * @param end    结束时间
     * @param ascend 是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByRevUidAndFlagAndSubmitTime(int uid, boolean flag, Date start, Date end, int number, int size, boolean ascend);

    /**
     * 根据期刊id获取稿件分页
     *
     * @param jid    期刊id
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByJid(int jid, int number, int size, boolean ascend);

    /**
     * 更新稿件记录
     *
     * @param post 要更新的稿件对象
     */
    void updatePost(Post post);

    /**
     * 通过id删除稿件记录
     *
     * @param id 稿件id
     */
    void deletePost(int id);
}
