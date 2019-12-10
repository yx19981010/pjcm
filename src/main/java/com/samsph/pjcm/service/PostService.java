package com.samsph.pjcm.service;

import com.samsph.pjcm.config.constant.PostStatus;
import com.samsph.pjcm.model.Post;
import com.samsph.pjcm.query.PostQuery;
import org.springframework.data.domain.Page;

import java.util.List;

/**
 * 投稿表service层接口
 *
 * @author hujiahao
 */
public interface PostService {
    /**
     * 新增一条投稿记录
     *
     * @param postQuery 投稿请求
     * @param creatorId 投稿人id
     * @return 投稿实体
     */
    Post savePost(PostQuery postQuery, int creatorId);


    /**
     * 根据id获取投稿
     *
     * @param id 投稿id
     * @return 投稿实体对象
     */
    Post getPost(int id);


    /**
     * 以分页形式获取投稿人的投稿列表
     *
     * @param uid    投稿人id
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 投稿分页
     */
    Page<Post> getAllByCtr(int uid, int number, int size, boolean ascend);

    /**
     * 以分页形式获取投稿人某一状态的投稿列表
     *
     * @param uid    投稿人id
     * @param status 稿件状态
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 投稿分页
     */
    Page<Post> getAllByCtrAndStatus(int uid, PostStatus status, int number, int size, boolean ascend);

    /**
     * 以分页形式获取编辑负责编辑的投稿列表
     *
     * @param uid    编辑id
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 投稿分页
     */
    Page<Post> getAllByEd(int uid, Integer number, Integer size, Boolean ascend);

    /**
     * 以分页形式获编辑负责编辑的某一状态的投稿列表
     *
     * @param uid    编辑id
     * @param status 稿件状态
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 投稿分页
     */
    Page<Post> getAllByEdAndStatus(int uid, PostStatus status, Integer number, Integer size, Boolean ascend);

    /**
     * 以分页形式获取审稿人未答复的投稿列表
     *
     * @param uid    编辑id
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 投稿分页
     */
    Page<Post> getAllByRevUnanswer(int uid, Integer number, Integer size, Boolean ascend);

    /**
     * 以分页形式获取审稿人已接受且正处于审稿中的稿件列表
     *
     * @param uid    审稿人id
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 投稿分页
     */
    Page<Post> getAllByRev(int uid, Integer number, Integer size, Boolean ascend);

    /**
     * 以分页形式获取审稿人需要审稿的稿件列表
     *
     * @param uid            审稿人id
     * @param reviewRequired 是否需要审稿
     * @param number         页号
     * @param size           页面大小
     * @param ascend         是否升序
     * @return 投稿分页
     */
    Page<Post> getAllRequiredToReview(int uid, boolean reviewRequired, Integer number, Integer size, Boolean ascend);

    /**
     * 获取某一期刊包含的投稿列表
     *
     * @param jid    期刊id
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 投稿分页
     */
    Page<Post> getAllByJid(int jid, Integer number, Integer size, Boolean ascend);

    /**
     * 更新投稿记录
     *
     * @param post 要更新的投稿对象
     */
    void updatePost(Post post);


    /**
     * 通过id删除投稿记录
     *
     * @param id 投稿id
     */
    void deletePost(int id);
}
