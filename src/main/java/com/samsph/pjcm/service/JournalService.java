package com.samsph.pjcm.service;

import com.samsph.pjcm.config.PageData;
import com.samsph.pjcm.query.JournalQuery;
import com.samsph.pjcm.vo.*;

/**
 * 期刊表service层接口
 *
 * @author hujiahao
 */
public interface JournalService {
    /**
     * 新增一条期刊记录
     *
     * @param journal 要创建的期刊对象
     *
     * @return JournalSimpleVO
     */
    JournalSimpleVO saveJournal(JournalQuery journal, int creatorId);

    /**
     * 通过id删除期刊记录
     *
     * @param id 期刊id
     */
    void deleteJournal(int id);

    /**
     * 更新一条期刊记录
     *
     * @param journal 要更新的期刊对象
     */
    void updateJournal(JournalQuery journal);

    /**
     * 根据id获取一条期刊数据
     *
     * @param id 期刊id
     *
     * @return JournalVO
     */
    JournalVO  getJournal(int id);


    /**
     * 根据年月卷期号获取一条期刊数据
     *
     * @param year 年号
     * @param month 月号
     * @param vol 卷号
     * @param no 期号
     *
     * @return JournalVO
     */
    JournalVO getJournal(int year, int month, int vol, int no);


    /**
     * 以分页形式获取期刊列表
     *
     * @param page 分页页号
     * @param size 分页页面大小
     * @param sortBy 如何排序
     * @param ascend 是否升序
     * @return PageData
     */
    PageData getAll(int page, int size, String sortBy, boolean ascend);
}