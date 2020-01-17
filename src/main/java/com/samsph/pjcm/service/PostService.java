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
     * @param uid      投稿人id
     * @param statuses 稿件状态列表
     * @param number   页号
     * @param size     页面大小
     * @param ascend   是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByCtrUidAndStatus(int uid, List<Integer> statuses, int number, int size, boolean ascend);


    /**
     * 根据投稿人id和稿件状态获取某段时间内提交的稿件分页
     *
     * @param uid      投稿人id
     * @param statuses 稿件状态列表
     * @param start    开始时间
     * @param end      结束时间
     * @param number   页号
     * @param size     页面大小
     * @param ascend   是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByCtrUidAndStatusAndSubmitTime(int uid, List<Integer> statuses, Date start, Date end, int number, int size, boolean ascend);

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
     * @param uid      编辑id
     * @param statuses 稿件状态列表
     * @param number   页号
     * @param size     页面大小
     * @param ascend   是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByEdUidAndStatus(int uid, List<Integer> statuses, int number, int size, boolean ascend);

    /**
     * 根据编辑id和稿件状态获取其负责编辑的某段时间内提交的稿件分页
     *
     * @param uid      编辑id
     * @param statuses 稿件状态列表
     * @param start    开始时间
     * @param end      结束时间
     * @param number   页号
     * @param size     页面大小
     * @param ascend   是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByEdUidAndStatusAndSubmitTime(int uid, List<Integer> statuses, Date start, Date end, int number, int size, boolean ascend);

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

    /**
     * 根据状态获得所有稿件
     *
     * @param statuses 稿件状态列表
     * @param number   页号
     * @param size     页面大小
     * @param ascend   是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByStatus(List<Integer> statuses, Integer number, Integer size, Boolean ascend);

    /**
     * 根据状态和一作单位获得所有稿件
     *
     * @param statuses    稿件状态列表
     * @param fAuEmployer 一作工作单位
     * @param number      页号
     * @param size        页面大小
     * @param ascend      是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByStatusAndAuEmployer(List<Integer> statuses, String fAuEmployer, Integer number, Integer size, Boolean ascend);

    /**
     * 根据状态和一作姓名获得所有稿件
     *
     * @param statuses 稿件状态列表
     * @param fAuName  一作姓名
     * @param number   页号
     * @param size     页面大小
     * @param ascend   是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByStatusAndAuName(List<Integer> statuses, String fAuName, Integer number, Integer size, Boolean ascend);

    /**
     * 根据状态、一作姓名和一作单位获得稿件列表
     *
     * @param statuses    稿件状态列表
     * @param fAuName     一作姓名
     * @param fAuEmployer 一作单位
     * @param number      页号
     * @param size        页面大小
     * @param ascend      是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByStatusAndAuNameAndAuEmployer(List<Integer> statuses, String fAuName, String fAuEmployer, Integer number, Integer size, Boolean ascend);

    /**
     * 根据状态和提交时间获得稿件分页
     *
     * @param statuses 状态列表
     * @param start    开始时间
     * @param end      结束时间
     * @param number   页号
     * @param size     页面大小
     * @param ascend   是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByStatusAndSubmitTime(List<Integer> statuses, Date start, Date end, Integer number, Integer size, Boolean ascend);

    /**
     * 根据状态、一作单位和提交时间获得稿件分页
     *
     * @param statuses    状态列表
     * @param fAuEmployer 一作单位
     * @param start       开始时间
     * @param end         结束时间
     * @param number      页号
     * @param size        页面大小
     * @param ascend      是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByStatusAndAuEmployerAndSubmitTime(List<Integer> statuses, String fAuEmployer, Date start, Date end, Integer number, Integer size, Boolean ascend);

    /**
     * 根据状态、一作姓名和提交时间获得稿件分页
     *
     * @param statuses 状态列表
     * @param fAuName  一作姓名
     * @param start    开始时间
     * @param end      结束时间
     * @param number   页号
     * @param size     页面大小
     * @param ascend   是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByStatusAndAuNameAndSubmitTime(List<Integer> statuses, String fAuName, Date start, Date end, Integer number, Integer size, Boolean ascend);

    /**
     * 根据状态、一作姓名、一作工作单位和提交时间获得稿件分页
     *
     * @param statuses    状态列表
     * @param fAuName     一作姓名
     * @param fAuEmployer 一作工作单位
     * @param start       开始时间
     * @param end         结束时间
     * @param number      页号
     * @param size        页面大小
     * @param ascend      是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByStatusAndAuNameAndAuEmployerAndSubmitTime(List<Integer> statuses, String fAuName, String fAuEmployer, Date start, Date end, Integer number, Integer size, Boolean ascend);

    /**
     * 根据稿件状态、缴费证明上传时间获得稿件分页
     *
     * @param statuses 稿件状态列表
     * @param start    开始时间
     * @param end      结束时间
     * @param number   页号
     * @param size     页面大小
     * @param ascend   是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByStatusAndCertificateUploadTime(List<Integer> statuses, Date start, Date end, Integer number, Integer size, Boolean ascend);

    /**
     * 根据稿件状态、一作工作单位、缴费证明上传时间获得稿件分页
     *
     * @param statuses    稿件状态列表
     * @param fAuEmployer 一作工作单位
     * @param start       开始时间
     * @param end         结束时间
     * @param number      页号
     * @param size        页面大小
     * @param ascend      是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByStatusAndAuEmployerAndCertificateUploadTime(List<Integer> statuses, String fAuEmployer, Date start, Date end, Integer number, Integer size, Boolean ascend);

    /**
     * 根据稿件状态、一作姓名、缴费证明上传时间获得稿件分页
     *
     * @param statuses 稿件状态列表
     * @param fAuName  一作姓名
     * @param start    开始时间
     * @param end      结束时间
     * @param number   页号
     * @param size     分页大小
     * @param ascend   是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByStatusAndAuNameAndCertificateUploadTime(List<Integer> statuses, String fAuName, Date start, Date end, Integer number, Integer size, Boolean ascend);

    /**
     * 根据稿件状态、一作姓名、一作工作单位、缴费证明上传时间获得稿件分页
     * 
     * @param statuses    稿件状态列表
     * @param fAuName     一作姓名
     * @param fAuEmployer 一作工作单位
     * @param start       开始时间
     * @param end         结束时间
     * @param number      页号
     * @param size        分页大小
     * @param ascend      是否升序
     * @return 稿件分页
     */
    Page<Post> getAllByStatusAndAuNameAndAuEmployerAndCertificateUploadTime(List<Integer> statuses, String fAuName, String fAuEmployer, Date start, Date end, Integer number, Integer size, Boolean ascend);
}
