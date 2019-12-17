package com.samsph.pjcm.service;

import com.samsph.pjcm.model.Announcement;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.util.Optional;

public interface AnnouncementService {
    /**
     * 添加公告
     * @param announcement 公告实体
     */
    void addAnnouncement(Announcement announcement);

    /**
     * 更新公告
     * @param announcement 公告实体
     */
    void updateAnnouncement(Announcement announcement);

    /**
     * 删除公告
     * @param id 公告id
     */
     void deleteAnnouncement(int id);

    /**
     * 得到公告列表
     * @param pageRequest 分页请求
     * @return 分页的公告列表
     */
     Page<Announcement> findAnnouncements(PageRequest pageRequest);

    /**
     * 得到单个公告
     * @param id 公告id
     * @return 单个公告
     */
     Optional<Announcement> findAnnouncement(int id);
}
