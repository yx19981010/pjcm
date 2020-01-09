package com.samsph.pjcm.service;

import com.samsph.pjcm.model.PostNoOffset;

/**
 * @author hujiahao
 */
public interface PostNoOffsetService {
    /**
     * 获取该年已用序号，并自增1
     *
     * @param year 年号
     * @return 该年投稿编号已用序号
     */
    int getOffsetAndIncrease(int year);
}
