package com.samsph.pjcm.service;

import com.samsph.pjcm.config.PageData;
import com.samsph.pjcm.config.constant.EditorRejectType;
import com.samsph.pjcm.config.constant.PostStatus;
import com.samsph.pjcm.query.PostQuery;
import com.samsph.pjcm.query.PostReceiptQuery;
import com.samsph.pjcm.vo.PostSimpleVO;
import com.samsph.pjcm.vo.PostVO;

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
     * @return PostSimpleVO
     */
    PostSimpleVO savePost(PostQuery postQuery, int creatorId);

    /**
     * 设置提交时间
     *
     * @param id 稿件id
     */
    void setSubmitTime(int id);

    /**
     * 设置编辑
     *
     * @param postId   稿件id
     * @param editorId 编辑id
     */
    void setEditor(int postId, int editorId);



    /**
     * 更新一条投稿记录（基本信息）
     *
     * @param postQuery 要更新的期刊请求对象
     */
    void updateBasicInfo(PostQuery postQuery);

    /**
     * 检查一条投稿记录的状态
     *
     * @param id         投稿id
     * @param postStatus 检查该投稿是否为此状态
     */
    void checkStatus(int id, PostStatus postStatus);

    /**
     * 更新一条投稿记录的状态
     *
     * @param id         投稿id
     * @param postStatus 要更新成为的投稿状态
     */
    void updateStatus(int id, PostStatus postStatus);

    /**
     * 检查并更新一条投稿记录的状态
     *
     * @param id        投稿id
     * @param oldStatus 检查该投稿是否为此状态
     * @param newStatus 要更新成为的投稿状态
     */
    void checkAndUpdateStatus(int id, PostStatus oldStatus, PostStatus newStatus);

    /**
     * 编辑初审否决；
     *
     * @param id      投稿id
     * @param type    拒绝类型
     * @param comment 拒绝意见
     */
    void rejectPost(int id, EditorRejectType type, String comment);

    /**
     * 设置版面费
     *
     * @param id  稿件id
     * @param fee 版面费
     */
    public void setLayoutFee(int id, double fee);

    /**
     * 更新收据信息
     *
     * @param postReceiptQuery 收据请求对象
     */
    void updateReceiptInfo(PostReceiptQuery postReceiptQuery);

    /**
     * 检查投稿人投稿信息是否完整
     * @param id 投稿id
     */
    void checkPostInfo(Integer id);


    /**
     * 检查投稿人支付信息是否完整
     *
     * @param id 投稿id
     */
    void checkPaymentInfo(Integer id);

    /**
     * 根据id获取一条投稿数据
     *
     * @param id 投稿id
     * @return PostVO
     */
    PostVO getPost(int id);

    /**
     * 以分页形式获取投稿列表
     *
     * @param page   页号
     * @param size   页面大小
     * @param sort   排序字段
     * @param ascend 是否升序
     * @return PageData
     */
    PageData getAll(int page, int size, String sort, boolean ascend);

    /**
     * 以分页形式获取某一状态的投稿列表
     *
     * @param status 稿件状态
     * @param page   页号
     * @param size   页面大小
     * @param sort   排序字段
     * @param ascend 是否升序
     * @return PageData
     */
    PageData getAllByStatus(PostStatus status, int page, int size, String sort, boolean ascend);

    /**
     * 通过id删除投稿记录
     *
     * @param id 投稿id
     */
    void deletePost(int id);

    /**
     * 将某一稿件归于某一期期刊
     *
     * @param id  投稿id
     * @param jid 期刊id
     */
    void setJournal(int id, int jid);
}
